#include "Optimizer.h"
#include "llvm/Analysis/CGSCCPassManager.h"
#include "llvm/Analysis/LoopAnalysisManager.h"
#include "llvm/Analysis/Passes.h"
#include "llvm/IR/Analysis.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Pass.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
#include <algorithm>

namespace {
struct RemoveNOOPS : llvm::PassInfoMixin<RemoveNOOPS> {
    void removeNoOps(llvm::Function& func) {
        for (auto& bb : func) {
            for (auto& instr : llvm::make_early_inc_range(bb)) {
                if (auto* gep = dyn_cast<llvm::GetElementPtrInst>(&instr)) {
                    if (std::all_of(gep->idx_begin(), gep->idx_end(), [](auto& idx) {
                            if (auto* constInt = dyn_cast<llvm::ConstantInt>(&idx)) {
                                return constInt->getSExtValue() == 0;
                            }
                            return false;
                        })) {
                        gep->replaceAllUsesWith(gep->getPointerOperand());
                        gep->eraseFromParent();
                    }
                }
            }
        }
    }
    llvm::PreservedAnalyses run(llvm::Function& F, llvm::FunctionAnalysisManager& FAM) {
        removeNoOps(F);
        return llvm::PreservedAnalyses::none();
    }
    static bool isRequired() { return true; }
};

struct ConstantFolding : llvm::PassInfoMixin<ConstantFolding> {
    void doConstantFolding(llvm::Instruction* instr) {
        auto is_const = [](llvm::Instruction& instr) {
            return std::all_of(instr.op_begin(), instr.op_end(),
                               [](llvm::Value* v) { return dyn_cast<llvm::Constant>(v); });
        };
        if (!is_const(*instr))
            return;
        auto* valToReplace = [instr]() -> llvm::Value* {
            switch (instr->getOpcode()) {
            case llvm::Instruction::Add:
                return llvm::ConstantInt::get(llvm::Type::getInt64Ty(instr->getContext()),
                                              cast<llvm::ConstantInt>(instr->getOperand(0))->getZExtValue() +
                                                  cast<llvm::ConstantInt>(instr->getOperand(1))->getZExtValue());
            default:
                return nullptr;
            }
        }();
        if (!valToReplace)
            return;
        instr->replaceAllUsesWith(valToReplace);
        instr->eraseFromParent();

        for (auto& use : valToReplace->uses()) {
            if (auto* useInstr = dyn_cast<llvm::Instruction>(use))
                doConstantFolding(useInstr);
        }
    }

    void foldConstants(llvm::Function& func, llvm::FunctionAnalysisManager& FAM) {
        llvm::DominatorTree* DT = &FAM.getResult<llvm::DominatorTreeAnalysis>(func);
        for (llvm::DomTreeNodeBase<llvm::BasicBlock>* node : llvm::post_order(DT)) {
            llvm::BasicBlock* bb = node->getBlock();
            auto rinst = llvm::make_range(bb->rbegin(), bb->rend());
            for (llvm::Instruction& inst : llvm::make_early_inc_range(rinst)) {
                doConstantFolding(&inst);
            }
        }
    }
    llvm::PreservedAnalyses run(llvm::Function& F, llvm::FunctionAnalysisManager& FAM) {
        foldConstants(F, FAM);
        return llvm::PreservedAnalyses::none();
    }
    static bool isRequired() { return true; }
};

struct TrivialDCE : llvm::PassInfoMixin<TrivialDCE> {
    void trivialDCE(llvm::Function& func, llvm::FunctionAnalysisManager& FAM) {
        llvm::DominatorTree* DT = &FAM.getResult<llvm::DominatorTreeAnalysis>(func);
        for (llvm::DomTreeNodeBase<llvm::BasicBlock>* node : llvm::post_order(DT)) {
            llvm::BasicBlock* bb = node->getBlock();
            auto rinst = llvm::make_range(bb->rbegin(), bb->rend());
            for (llvm::Instruction& inst : llvm::make_early_inc_range(rinst)) {
                if (inst.isSafeToRemove() && !inst.mayHaveSideEffects() && inst.user_empty()) {
                    inst.eraseFromParent();
                }
            }
        }
    }
    llvm::PreservedAnalyses run(llvm::Function& F, llvm::FunctionAnalysisManager& FAM) {
        trivialDCE(F, FAM);
        return llvm::PreservedAnalyses::none();
    }
    static bool isRequired() { return true; }
};

struct MemToReg : llvm::PassInfoMixin<MemToReg> {
    // TODO: Make this more extensive. Currently only covers easy cases
    bool allUsesAreLoadStore(llvm::AllocaInst* alloca) {
        return std::all_of(alloca->user_begin(), alloca->user_end(), [](llvm::User* user) {
            return dyn_cast<llvm::LoadInst>(user) || dyn_cast<llvm::StoreInst>(user);
        });
    }

    llvm::BasicBlock* allUsesInOneBlock(llvm::AllocaInst* alloca) {
        llvm::BasicBlock* block = nullptr;
        for (auto* user : alloca->users()) {
            if (auto* use_instr = llvm::dyn_cast<llvm::Instruction>(user)) {
                if (!block)
                    block = use_instr->getParent();
                else {
                    if (block != use_instr->getParent())
                        return nullptr;
                }
            } else {
                return nullptr; // wtf uses us?
            }
        }
        return block;
    }

    llvm::StoreInst* onlyOneStore(llvm::AllocaInst* alloca) {
        llvm::StoreInst* foundStore = nullptr;
        for (auto* user : alloca->users()) {
            if (auto* store = dyn_cast<llvm::StoreInst>(user)) {
                if (foundStore) {
                    return nullptr;
                }
                foundStore = store;
            } else if (!dyn_cast<llvm::LoadInst>(user)) {
                return nullptr; // other kind of use -> not trivial case
            }
        }
        return foundStore;
    }

    void memToReg(llvm::Function& func, llvm::FunctionAnalysisManager& FAM) {
        if (func.isDeclaration())
            return;
        auto& entryBlock = func.getEntryBlock();
        std::vector<llvm::AllocaInst*> allocas;
        for (auto& instr : entryBlock) {
            if (auto* alloca = dyn_cast<llvm::AllocaInst>(&instr)) {
                allocas.push_back(alloca);
            }
        }
        llvm::DominatorTree* DT = &FAM.getResult<llvm::DominatorTreeAnalysis>(func);
        for (auto* alloca : allocas) {
            if (auto* store = onlyOneStore(alloca)) {
                llvm::IRBuilder<> builder{store};
                auto* storee = store->getOperand(0);
                assert(storee != store->getPointerOperand());
                if (!storee->getType()->isPointerTy() && storee->getType()->getIntegerBitWidth() != 64) {
                    storee = builder.CreateSExt(storee, llvm::Type::getInt64Ty(store->getContext()));
                }
                if(storee->getType()->isPointerTy()) {
                    storee = builder.CreatePtrToInt(storee, llvm::Type::getInt64Ty(store->getContext())  );
                }
                for (auto* user : llvm::make_early_inc_range(alloca->users())) {
                    if (user == store)
                        continue;
                    if (auto* load = dyn_cast<llvm::LoadInst>(user)) {
                        if(!DT->dominates(store, load)) {
                            load->replaceAllUsesWith(llvm::UndefValue::get(load->getType()));
                            load->eraseFromParent();
                            continue;
                        }
                        auto* localLoad = storee;
                        assert(load->getType()->isIntegerTy());
                        if (load->getType()->getIntegerBitWidth() != 64) {
                            builder.SetInsertPoint(load);
                            localLoad = builder.CreateTrunc(storee, load->getType());
                        }
                        load->replaceAllUsesWith(localLoad);
                        load->eraseFromParent();
                    }
                }
                store->eraseFromParent();
                alloca->eraseFromParent();
             }
        }
    }

    llvm::PreservedAnalyses run(llvm::Function& F, llvm::FunctionAnalysisManager& FAM) {
        memToReg(F, FAM);
        return llvm::PreservedAnalyses::none();
    }
    static bool isRequired() { return true; }
};

} // namespace

void optimize(llvm::Module* module) {
    // reference: https://github.com/csb6/bluebird/blob/master/src/optimizer.cpp
    llvm::ModulePassManager module_manager;
    llvm::FunctionPassManager funct_manager;
    funct_manager.addPass(RemoveNOOPS());
    funct_manager.addPass(TrivialDCE());
    funct_manager.addPass(MemToReg());
    funct_manager.addPass(ConstantFolding());
    module_manager.addPass(llvm::createModuleToFunctionPassAdaptor(std::move(funct_manager)));

    llvm::PassBuilder pass_builder;
    llvm::LoopAnalysisManager loop_analysis;
    llvm::FunctionAnalysisManager funct_analysis;
    llvm::CGSCCAnalysisManager cg_analysis;
    llvm::ModuleAnalysisManager module_analysis;

    pass_builder.registerFunctionAnalyses(funct_analysis);
    pass_builder.registerModuleAnalyses(module_analysis);
    pass_builder.registerLoopAnalyses(loop_analysis);
    pass_builder.registerCGSCCAnalyses(cg_analysis);
    pass_builder.crossRegisterProxies(loop_analysis, funct_analysis, cg_analysis, module_analysis);

    module_manager.run(*module, module_analysis);
    if (llvm::verifyModule(*module, &llvm::errs())) {
        module->print(llvm::errs(), nullptr);
        throw std::runtime_error("Invalid IR!");
    }
}
