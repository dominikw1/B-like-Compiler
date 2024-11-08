#pragma once
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Value.h"
#include <unordered_map>
#include <unordered_set>

class ValueTracker {
    std::unordered_map<std::string_view, std::unordered_map<const llvm::BasicBlock*, llvm::Value*>> currentDef;
    std::unordered_set<const llvm::BasicBlock*> sealed;
    std::unordered_map<const llvm::BasicBlock*, std::unordered_map<std::string_view, llvm::Value*>> incompletePhis;

    llvm::LLVMContext& context;
    llvm::IRBuilder<> builder;

    llvm::Value* reduceTrivialPhi(llvm::PHINode* phi);
    llvm::Value* addPhiOperands(std::string_view var, llvm::PHINode* phi, llvm::BasicBlock* block);
    llvm::Value* readVariableRecursive(std::string_view var, llvm::BasicBlock* block);

  public:
    ValueTracker(llvm::LLVMContext& context) : context{context}, builder{context} {}

    void sealBlock(llvm::BasicBlock* block);
    llvm::Value* readVariable(std::string_view var, llvm::BasicBlock* block);
    void writeVariable(std::string_view var, const llvm::BasicBlock* block, llvm::Value* value);
};