#include "InstructionSelector.h"
#include <cstdint>
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
    virtual void replace(llvm::Module& module, llvm::Value* root) = 0;
    virtual std::uint16_t getSize() = 0;
};

static constexpr llvm::Function* getInstruction(llvm::Module& module, llvm::StringRef name, llvm::Type* retType,
                                                std::vector<llvm::Type*> args) {
    auto* calledFunc = module.getFunction(name);
    if (!calledFunc) {
        // implicit function declaration
        auto type = llvm::FunctionType::get(retType, args, false);
        calledFunc = llvm::cast<llvm::Function>(module.getOrInsertFunction(name, type).getCallee());
    }
    return calledFunc;
}

struct ADD64rr : public Pattern {
    bool matches(llvm::Value* root) override {
        if (auto* rootInst = dyn_cast<llvm::Instruction>(root)) {
            if (rootInst->getOpcode() == llvm::Instruction::Add) {
                return notConst(rootInst->getOperand(0)) && notConst(rootInst->getOperand(1)); // else use imm
            }
        }
        return false;
    }
    void markCovered(llvm::Value* root, std::unordered_set<llvm::Value*>& covered) override { covered.insert(root); }

    void replace(llvm::Module& module, llvm::Value* rootVal) override {
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
                return notConst(root->getOperand(0)) != notConst(root->getOperand(1));
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
    void replace(llvm::Module& module, llvm::Value* rootVal) override {
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

std::vector<std::unique_ptr<Pattern>> fillPatterns() noexcept {
    std::vector<std::unique_ptr<Pattern>> patterns;
    patterns.push_back(std::make_unique<ADD64rr>());
    patterns.push_back(std::make_unique<ADD64ri>());
    std::sort(patterns.begin(), patterns.end(),
              [](const std::unique_ptr<Pattern>& p1, const std::unique_ptr<Pattern>& p2) {
                  return p1->getSize() > p2->getSize();
              });
    return patterns;
}

static std::vector<std::unique_ptr<Pattern>> patterns = fillPatterns();

struct Context {
    std::vector<std::pair<llvm::Value*, Pattern*>> selectionMap;
    std::unordered_set<llvm::Value*> covered;
};

// TODO: canonicalisation, such that no op has 2 constants

void selectInstruction(llvm::Instruction& instr, Context& context);

void selectValue(llvm::Value* val, Context& context) {
    // constants that werent picked up yet ig...
    if (context.covered.contains(val))
        return;
    if (auto* instr = dyn_cast<llvm::Instruction>(val))
        selectInstruction(*instr, context);
    else
        throw std::runtime_error("TODO: implement value selection");
}

void selectInstruction(llvm::Instruction& instr, Context& context) {
    if (context.covered.contains(&instr))
        return; // already covered - no need to select for it
    auto pattern =
        std::find_if(patterns.begin(), patterns.end(),
                     [instr = &instr](const std::unique_ptr<Pattern>& pattern) { return pattern->matches(instr); });
    if (pattern == patterns.end()) {
        instr.print(llvm::errs());
        throw std::runtime_error("could not select instruction");
    }
    context.selectionMap.emplace_back(&instr, pattern->get());
    (*pattern)->markCovered(&instr, context.covered);

    for (auto& operand : instr.operands()) {
        selectValue(operand, context);
    }
}

void selectBlock(llvm::BasicBlock& block, Context& context) {
    for (auto& instr : llvm::reverse(block)) {
        selectInstruction(instr, context);
    }
}

void selectFunction(llvm::Function& func) {
    Context context;

    // todo: maybe postdominator tree order?
    for (auto& block : llvm::reverse(func)) {
        selectBlock(block, context);
    }
    for (auto& [val, pattern] : std::views::reverse(context.selectionMap)) {
        pattern->replace(*func.getParent(), val);
    }
    func.print(llvm::errs());
}

void doInstructionSelection(llvm::Module& module) {
    for (auto& func : module) {
        selectFunction(func);
    }
}