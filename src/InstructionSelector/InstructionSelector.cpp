#include "InstructionSelector.h"
#include <cstdint>
#include <iostream>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Module.h>
#include <ranges>
#include <unordered_map>
#include <unordered_set>

constexpr bool notConst(llvm::Value* v) { return !dyn_cast<llvm::Constant>(v); }
constexpr bool isConst(llvm::Value* v) { return nullptr != dyn_cast<llvm::Constant>(v); }

struct Pattern {
    virtual bool matches(llvm::Value* root) = 0;
    virtual void markCovered(llvm::Value* root, std::unordered_set<llvm::Value*>& covered) = 0;
    virtual void replace(llvm::Module& module, llvm::Value* root, llvm::Value* parent) = 0;
    virtual std::uint16_t getSize() = 0;
};

static constexpr llvm::Function* getInstruction(llvm::Module& module, llvm::StringRef name, llvm::Type* retType,
                                                std::vector<llvm::Type*> args, bool varags = false) {
    auto* calledFunc = module.getFunction(name);
    if (!calledFunc) {
        // implicit function declaration
        auto type = llvm::FunctionType::get(retType, args, varags);
        calledFunc = llvm::cast<llvm::Function>(module.getOrInsertFunction(name, type).getCallee());
    }
    return calledFunc;
}

struct ADD64rr : public Pattern {
    bool matches(llvm::Value* root) override {
        if (auto* rootInst = dyn_cast<llvm::Instruction>(root)) {
            if (rootInst->getOpcode() == llvm::Instruction::Add) {
                return true; // TODO: is this ok?
                //           return notConst(rootInst->getOperand(0)) && notConst(rootInst->getOperand(1)); // else use
                //           imm
            }
        }
        return false;
    }
    void markCovered(llvm::Value* root, std::unordered_set<llvm::Value*>& covered) override { covered.insert(root); }

    void replace(llvm::Module& module, llvm::Value* rootVal, llvm::Value* parent) override {
        auto* root = cast<llvm::Instruction>(rootVal);
        auto* call = llvm::IRBuilder<>{root}.CreateCall(getInstruction(module, "ADD64rr",
                                                                       llvm::Type::getInt64Ty(module.getContext()),
                                                                       {
                                                                           llvm::Type::getInt64Ty(module.getContext()),
                                                                           llvm::Type::getInt64Ty(module.getContext()),
                                                                       }),
                                                        {root->getOperand(0), root->getOperand(1)});
        root->replaceAllUsesWith(call);
        root->removeFromParent();
    }
    std::uint16_t getSize() override { return 1; } // just the plus
};

struct ADD64ri : public Pattern {
    bool matches(llvm::Value* rootVal) override {
        if (auto* root = dyn_cast<llvm::Instruction>(rootVal)) {
            if (root->getOpcode() == llvm::Instruction::Add) {
                // exactly one of them is const
                if (!(notConst(root->getOperand(0)) != notConst(root->getOperand(1)))) {
                    return false;
                }
                llvm::Value* immediate = root->getOperand(0);
                if (notConst(immediate)) {
                    immediate = root->getOperand(1);
                }
                auto* intVal = cast<llvm::ConstantInt>(immediate);
                auto rawVal = intVal->getSExtValue();
                return rawVal >= INT32_MIN && rawVal <= INT32_MAX;
            }
        }
        return false;
    }
    void markCovered(llvm::Value* rootVal, std::unordered_set<llvm::Value*>& covered) override {
        auto* root = cast<llvm::Instruction>(rootVal);
        covered.insert(root);
        if (isConst(root->getOperand(0)))
            covered.insert(root->getOperand(0));
        else
            covered.insert(root->getOperand(1));
    }
    void replace(llvm::Module& module, llvm::Value* rootVal, llvm::Value* parent) override {
        auto* root = cast<llvm::Instruction>(rootVal);
        llvm::Value* immediate = root->getOperand(0);
        llvm::Value* registerVal = root->getOperand(1);
        if (notConst(immediate)) {
            std::swap(immediate, registerVal);
        }
        auto* call = llvm::IRBuilder<>{root}.CreateCall(getInstruction(module, "ADD64ri",
                                                                       llvm::Type::getInt64Ty(module.getContext()),
                                                                       {
                                                                           llvm::Type::getInt64Ty(module.getContext()),
                                                                           llvm::Type::getInt64Ty(module.getContext()),
                                                                       }),
                                                        {root->getOperand(0), root->getOperand(1)});
        root->replaceAllUsesWith(call);
        root->removeFromParent();
    }
    std::uint16_t getSize() override { return 2; } // + and one constant
};

struct MOV64ri : public Pattern {
    bool matches(llvm::Value* rootVal) override { return isConst(rootVal) && rootVal->getType()->isIntegerTy(64); }
    void markCovered(llvm::Value* rootVal, std::unordered_set<llvm::Value*>& covered) override {
        covered.insert(rootVal);
    }
    void replace(llvm::Module& module, llvm::Value* rootVal, llvm::Value* parent) override {
        assert(parent);
        auto* parentInst = cast<llvm::Instruction>(parent);
        auto* call = llvm::IRBuilder<>{parentInst}.CreateCall(
            getInstruction(module, "MOV64ri", llvm::Type::getInt64Ty(module.getContext()),
                           {
                               llvm::Type::getInt64Ty(module.getContext()),
                           }),
            {rootVal});
        for (size_t i = 0; i < parentInst->getNumOperands(); ++i) {
            if (parentInst->getOperand(i) == rootVal) {
                parentInst->setOperand(i, call);
                break;
            }
        }
    }
    std::uint16_t getSize() override { return 1; } // one constant
};

struct SUB64rr : public Pattern {
    bool matches(llvm::Value* root) override {
        if (auto* rootInst = dyn_cast<llvm::Instruction>(root)) {
            if (rootInst->getOpcode() == llvm::Instruction::Sub) {
                return true; // TODO: is this ok?
                //           return notConst(rootInst->getOperand(0)) && notConst(rootInst->getOperand(1)); // else use
                //           imm
            }
        }
        return false;
    }
    void markCovered(llvm::Value* root, std::unordered_set<llvm::Value*>& covered) override { covered.insert(root); }

    void replace(llvm::Module& module, llvm::Value* rootVal, llvm::Value* parent) override {
        auto* root = cast<llvm::Instruction>(rootVal);
        auto* call = llvm::IRBuilder<>{root}.CreateCall(getInstruction(module, "SUB64rr",
                                                                       llvm::Type::getInt64Ty(module.getContext()),
                                                                       {
                                                                           llvm::Type::getInt64Ty(module.getContext()),
                                                                           llvm::Type::getInt64Ty(module.getContext()),
                                                                       }),
                                                        {root->getOperand(0), root->getOperand(1)});
        root->replaceAllUsesWith(call);
        root->removeFromParent();
    }
    std::uint16_t getSize() override { return 1; } // just the minus
};

struct SUB64ri : public Pattern {
    bool matches(llvm::Value* rootVal) override {
        if (auto* root = dyn_cast<llvm::Instruction>(rootVal)) {
            if (root->getOpcode() == llvm::Instruction::Sub) {
                // exactly one of them is const
                if (!(notConst(root->getOperand(0)) != notConst(root->getOperand(1)))) {
                    return false;
                }
                llvm::Value* immediate = root->getOperand(0);
                if (notConst(immediate)) {
                    immediate = root->getOperand(1);
                }
                auto* intVal = cast<llvm::ConstantInt>(immediate);
                auto rawVal = intVal->getSExtValue();
                return rawVal >= INT32_MIN && rawVal <= INT32_MAX;
            }
        }
        return false;
    }
    void markCovered(llvm::Value* rootVal, std::unordered_set<llvm::Value*>& covered) override {
        auto* root = cast<llvm::Instruction>(rootVal);
        covered.insert(root);
        if (isConst(root->getOperand(0)))
            covered.insert(root->getOperand(0));
        else
            covered.insert(root->getOperand(1));
    }
    void replace(llvm::Module& module, llvm::Value* rootVal, llvm::Value* parent) override {
        auto* root = cast<llvm::Instruction>(rootVal);
        llvm::Value* immediate = root->getOperand(0);
        llvm::Value* registerVal = root->getOperand(1);
        if (notConst(immediate)) {
            std::swap(immediate, registerVal);
        }
        auto* call = llvm::IRBuilder<>{root}.CreateCall(getInstruction(module, "SUB64ri",
                                                                       llvm::Type::getInt64Ty(module.getContext()),
                                                                       {
                                                                           llvm::Type::getInt64Ty(module.getContext()),
                                                                           llvm::Type::getInt64Ty(module.getContext()),
                                                                       }),
                                                        {root->getOperand(0), root->getOperand(1)});
        root->replaceAllUsesWith(call);
        root->removeFromParent();
    }
    std::uint16_t getSize() override { return 2; } // + and one constant
};

struct XOR64rr : public Pattern {
    bool matches(llvm::Value* root) override {
        if (auto* rootInst = dyn_cast<llvm::Instruction>(root)) {
            if (rootInst->getOpcode() == llvm::Instruction::Xor) {
                return true; // TODO: is this ok?
                //           return notConst(rootInst->getOperand(0)) && notConst(rootInst->getOperand(1)); // else use
                //           imm
            }
        }
        return false;
    }
    void markCovered(llvm::Value* root, std::unordered_set<llvm::Value*>& covered) override { covered.insert(root); }

    void replace(llvm::Module& module, llvm::Value* rootVal, llvm::Value* parent) override {
        auto* root = cast<llvm::Instruction>(rootVal);
        auto* call = llvm::IRBuilder<>{root}.CreateCall(getInstruction(module, "XOR64rr",
                                                                       llvm::Type::getInt64Ty(module.getContext()),
                                                                       {
                                                                           llvm::Type::getInt64Ty(module.getContext()),
                                                                           llvm::Type::getInt64Ty(module.getContext()),
                                                                       }),
                                                        {root->getOperand(0), root->getOperand(1)});
        root->replaceAllUsesWith(call);
        root->removeFromParent();
    }
    std::uint16_t getSize() override { return 1; } // just the minus
};

struct XOR64ri : public Pattern {
    bool matches(llvm::Value* rootVal) override {
        if (auto* root = dyn_cast<llvm::Instruction>(rootVal)) {
            if (root->getOpcode() == llvm::Instruction::Xor) {
                // exactly one of them is const
                if (!(notConst(root->getOperand(0)) != notConst(root->getOperand(1)))) {
                    return false;
                }
                llvm::Value* immediate = root->getOperand(0);
                if (notConst(immediate)) {
                    immediate = root->getOperand(1);
                }
                auto* intVal = cast<llvm::ConstantInt>(immediate);
                auto rawVal = intVal->getSExtValue();
                return rawVal >= INT32_MIN && rawVal <= INT32_MAX;
            }
        }
        return false;
    }
    void markCovered(llvm::Value* rootVal, std::unordered_set<llvm::Value*>& covered) override {
        auto* root = cast<llvm::Instruction>(rootVal);
        covered.insert(root);
        if (isConst(root->getOperand(0))) {
            covered.insert(root->getOperand(0));
        } else {
            covered.insert(root->getOperand(1));
        }
    }
    void replace(llvm::Module& module, llvm::Value* rootVal, llvm::Value* parent) override {
        auto* root = cast<llvm::Instruction>(rootVal);
        llvm::Value* immediate = root->getOperand(0);
        llvm::Value* registerVal = root->getOperand(1);
        if (notConst(immediate)) {
            std::swap(immediate, registerVal);
        }

        auto* call = llvm::IRBuilder<>{root}.CreateCall(getInstruction(module, "XOR64ri",
                                                                       llvm::Type::getInt64Ty(module.getContext()),
                                                                       {
                                                                           llvm::Type::getInt64Ty(module.getContext()),
                                                                           llvm::Type::getInt64Ty(module.getContext()),
                                                                       }),
                                                        {root->getOperand(0), root->getOperand(1)});
        root->replaceAllUsesWith(call);
        root->removeFromParent();
    }
    std::uint16_t getSize() override { return 2; }
};

struct Compare64rr : public Pattern {
    bool matches(llvm::Value* root) override {
        if (auto* rootInst = dyn_cast<llvm::Instruction>(root)) {
            if (rootInst->getOpcode() == llvm::Instruction::ICmp) {
                return true; // TODO: is this ok?
                //           return notConst(rootInst->getOperand(0)) && notConst(rootInst->getOperand(1)); // else use
                //           imm
            }
        }
        return false;
    }
    void markCovered(llvm::Value* root, std::unordered_set<llvm::Value*>& covered) override { covered.insert(root); }

    void replace(llvm::Module& module, llvm::Value* rootVal, llvm::Value* parent) override {
        auto* root = cast<llvm::Instruction>(rootVal);
        auto* cmpInst = cast<llvm::ICmpInst>(rootVal);
        int64_t cond_code = 0;
        switch (cmpInst->getPredicate()) {
        case llvm::ICmpInst::ICMP_EQ:
            cond_code = 4;
            break;
        default:
            throw std::runtime_error("unknown icmp");
        }

        llvm::IRBuilder<> builder{root};
        // cmp
        // setcc
        // andi 1
        auto* cmp = builder.CreateCall(getInstruction(module, "CMP64rr", llvm::Type::getVoidTy(module.getContext()),
                                                      {
                                                          llvm::Type::getInt64Ty(module.getContext()),
                                                          llvm::Type::getInt64Ty(module.getContext()),
                                                      }),
                                       {root->getOperand(0), root->getOperand(1)});

        auto* setcc =
            builder.CreateCall(getInstruction(module, "SETcc8r", llvm::Type::getInt1Ty(module.getContext()),
                                              {
                                                  llvm::Type::getInt64Ty(module.getContext()),
                                              }),
                               {llvm::ConstantInt::get(llvm::Type::getInt64Ty(module.getContext()), cond_code)});

        auto* andInst =
            builder.CreateCall(getInstruction(module, "AND64ri", llvm::Type::getInt64Ty(module.getContext()),
                                              {
                                                  llvm::Type::getInt64Ty(module.getContext()),
                                                  llvm::Type::getInt64Ty(module.getContext()),
                                              }),
                               {setcc, llvm::ConstantInt::get(llvm::Type::getInt64Ty(module.getContext()), 1)});

        root->replaceAllUsesWith(cmp);
        root->removeFromParent();
    }
    std::uint16_t getSize() override { return 1; }
};

struct Zext : public Pattern {
    std::unordered_map<llvm::Value*, std::size_t> bitsExtendedFrom;
    bool matches(llvm::Value* root) override {
        if (auto* rootInst = dyn_cast<llvm::ZExtInst>(root)) {
            assert(rootInst->getDestTy() == llvm::Type::getInt64Ty(rootInst->getContext()));
            if (rootInst->getSrcTy() == llvm::Type::getInt1Ty(rootInst->getContext())) {
                bitsExtendedFrom[rootInst] = 1;
            } else {
                rootInst->print(llvm::errs());
                throw std::runtime_error("not implemeneted cast");
            }
            rootInst->print(llvm::errs());
            return true;
        }
        return false;
    }
    void markCovered(llvm::Value* root, std::unordered_set<llvm::Value*>& covered) override { covered.insert(root); }

    void replace(llvm::Module& module, llvm::Value* rootVal, llvm::Value* parent) override {
        auto* root = cast<llvm::ZExtInst>(rootVal);

        llvm::IRBuilder<> builder{root};
        // and with (1<< numBitsToKeep) -1

        auto* zext =
            builder.CreateCall(getInstruction(module, "AND64ri", llvm::Type::getVoidTy(module.getContext()),
                                              {
                                                  llvm::Type::getInt64Ty(module.getContext()),
                                                  llvm::Type::getInt64Ty(module.getContext()),
                                              }),
                               {root->getOperand(0), llvm::ConstantInt::get(llvm::Type::getInt64Ty(module.getContext()),
                                                                            (1ull << bitsExtendedFrom[root]) - 1)});

        root->replaceAllUsesWith(zext);
        root->removeFromParent();
    }
    std::uint16_t getSize() override { return 1; }
};

std::vector<std::unique_ptr<Pattern>> fillPatterns() noexcept {
    std::vector<std::unique_ptr<Pattern>> patterns;
    patterns.push_back(std::make_unique<ADD64rr>());
    patterns.push_back(std::make_unique<ADD64ri>());
    patterns.push_back(std::make_unique<MOV64ri>());
    patterns.push_back(std::make_unique<SUB64rr>());
    patterns.push_back(std::make_unique<SUB64ri>());
    patterns.push_back(std::make_unique<XOR64rr>());
    patterns.push_back(std::make_unique<XOR64ri>());
    patterns.push_back(std::make_unique<Compare64rr>());
    patterns.push_back(std::make_unique<Zext>());
    std::sort(patterns.begin(), patterns.end(),
              [](const std::unique_ptr<Pattern>& p1, const std::unique_ptr<Pattern>& p2) {
                  return p1->getSize() > p2->getSize();
              });
    return patterns;
}

static std::vector<std::unique_ptr<Pattern>> patterns = fillPatterns();

struct Context {
    std::vector<std::tuple<llvm::Value*, Pattern*, llvm::Value*>> selectionMap;
    // std::unordered_set<llvm::Value*> covered;
    std::unordered_set<llvm::Value*> selected;
};

// TODO: canonicalisation, such that no op has 2 constants

void selectInstruction(llvm::Instruction& instr, Context& context, std::unordered_set<llvm::Value*>& localCovered);

void selectValue(llvm::Value* val, Context& context, llvm::Value* parent,
                 std::unordered_set<llvm::Value*>& localCovered) {
    if (context.selected.contains(val))
        return;
    if (auto* instr = dyn_cast<llvm::Instruction>(val))
        selectInstruction(*instr, context, localCovered);
    else {
        if (notConst(val) || localCovered.contains(val)) {
            return; // TODO: fix this - there are parameters that are passed on stack...
        }
        // constants that werent picked up yet ig...
        auto pattern = std::find_if(patterns.begin(), patterns.end(),
                                    [val](const std::unique_ptr<Pattern>& pattern) { return pattern->matches(val); });
        if (pattern == patterns.end()) {
            val->print(llvm::errs());
            throw std::runtime_error("could not select Value");
        }
        context.selected.insert(val);
        context.selectionMap.emplace_back(val, pattern->get(), parent);
        (*pattern)->markCovered(val, localCovered);
    }
}

void selectInstruction(llvm::Instruction& instr, Context& context, std::unordered_set<llvm::Value*>& localCovered) {
    if (context.selected.contains(&instr))
        return; // already selected - no need to select for it
    if (instr.getOpcode() != llvm::Instruction::Ret && !localCovered.contains(&instr)) {
        auto pattern =
            std::find_if(patterns.begin(), patterns.end(),
                         [instr = &instr](const std::unique_ptr<Pattern>& pattern) { return pattern->matches(instr); });
        if (pattern == patterns.end()) {
            instr.print(llvm::errs());
            throw std::runtime_error("could not select instruction");
        }
        context.selected.insert(&instr);
        context.selectionMap.emplace_back(&instr, pattern->get(), nullptr);
        (*pattern)->markCovered(&instr, localCovered);
    }
    for (auto& operand : instr.operands()) {
        selectValue(operand, context, &instr, localCovered);
    }
}

void dfs(std::unordered_set<llvm::Value*>& visited, llvm::Value* curr) {
    if (visited.contains(curr)) {
        return;
    }
    visited.insert(curr);
    if (auto* instr = dyn_cast<llvm::Instruction>(curr)) {
        for (auto& op : instr->operands()) {
            dfs(visited, op);
        }
    }
}

std::vector<llvm::Instruction*> extractRoots(llvm::Function& func) {
    std::unordered_set<llvm::Value*> visited;
    std::vector<llvm::Instruction*> roots;
    for (auto& bb : llvm::reverse(func)) {
        for (auto& instr : llvm::reverse(bb)) {
            if (visited.contains(&instr)) {
                continue;
            }
            roots.push_back(&instr);
            dfs(visited, &instr);
        }
    }
    return roots;
}

void selectFunction(llvm::Function& func) {
    Context context;
    auto roots = extractRoots(func);
    for (auto& root : roots) {
        std::unordered_set<llvm::Value*> localCovered;
        selectInstruction(*root, context, localCovered);
    }

    for (auto& [val, pattern, parent] : std::views::reverse(context.selectionMap)) {
        pattern->replace(*func.getParent(), val, parent);
    }

    // i64 FRAME_SETUP(numStackArgs, stackFrameSize, stackArg0, stackArg1, ...)
    auto* frame_setup_func = getInstruction(*func.getParent(), "FRAME_SETUP", llvm::Type::getInt64Ty(func.getContext()),
                                            {
                                                llvm::Type::getInt64Ty(func.getContext()),
                                                llvm::Type::getInt64Ty(func.getContext()),
                                            },
                                            true);
    auto* frame_destroy_func =
        getInstruction(*func.getParent(), "FRAME_DESTROY", llvm::Type::getVoidTy(func.getContext()),
                       {
                           llvm::Type::getInt64Ty(func.getContext()),
                       });
    std::uint64_t stackSize = 10;
    auto* framepointer =
        llvm::IRBuilder<>{&func.getEntryBlock(), func.getEntryBlock().getFirstInsertionPt()}.CreateCall(
            frame_setup_func, {llvm::ConstantInt::get(llvm::Type::getInt64Ty(func.getContext()), stackSize),
                               llvm::ConstantInt::get(llvm::Type::getInt64Ty(func.getContext()), 0)});

    for (auto& bb : func) {
        for (auto& instr : llvm::make_early_inc_range(bb)) {
            if (instr.getOpcode() == llvm::Instruction::Ret) {
                llvm::IRBuilder<>{&instr}.CreateCall(frame_destroy_func, {framepointer});
            }
        }
    }
}

void doInstructionSelection(llvm::Module& module) {
    for (auto& func : module) {
        if (!func.isDeclaration())
            selectFunction(func);
    }
}