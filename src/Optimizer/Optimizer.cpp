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
namespace {
void removeNoOps(llvm::Module* module) {
    for (auto& func : *module) {
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
}

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
void foldConstants(llvm::Module& module, llvm::FunctionAnalysisManager& FAM) {
    for (auto& func : module) {
        FAM.invalidate(func, llvm::PreservedAnalyses::none());
        llvm::DominatorTree* DT = &FAM.getResult<llvm::DominatorTreeAnalysis>(func);
        for (llvm::DomTreeNodeBase<llvm::BasicBlock>* node : llvm::post_order(DT)) {
            llvm::BasicBlock* bb = node->getBlock();
            auto rinst = llvm::make_range(bb->rbegin(), bb->rend());
            for (llvm::Instruction& inst : llvm::make_early_inc_range(rinst)) {
                doConstantFolding(&inst);
            }
        }
    }
}
} // namespace

void trivialDCE(llvm::Module& module, llvm::FunctionAnalysisManager& FAM) {
    for (auto& func : module) {
        FAM.invalidate(func, llvm::PreservedAnalyses::none());
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
}

void optimize(llvm::Module* module) {
    /*
    TODO: figure out how to do this properly
    */
    llvm::FunctionAnalysisManager FAM;
    llvm::LoopAnalysisManager LAM;
    llvm::CGSCCAnalysisManager CGAM;
    llvm::ModuleAnalysisManager MAM;

    // Create the new pass manager builder.
    // Take a look at the PassBuilder constructor parameters for more
    // customization, e.g. specifying a TargetMachine or various debugging
    // options.
    llvm::PassBuilder PB;

    // Register all the basic analyses with the managers.
    PB.registerModuleAnalyses(MAM);
    PB.registerCGSCCAnalyses(CGAM);
    PB.registerFunctionAnalyses(FAM);
    PB.registerLoopAnalyses(LAM);
    PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);

    removeNoOps(module);

   // trivialDCE(*module, FAM);
    // foldConstants(*module, FAM);

    if (llvm::verifyModule(*module, &llvm::errs())) {
        module->print(llvm::errs(), nullptr);
        throw std::runtime_error("Invalid IR!");
    }
}