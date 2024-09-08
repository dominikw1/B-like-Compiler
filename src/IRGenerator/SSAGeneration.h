#pragma once
#include "CFG.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include <string_view>
#include <unordered_map>
#include <unordered_set>

struct IntermediateRepresentation {
    std::unique_ptr<llvm::LLVMContext> context;
    std::unique_ptr<llvm::Module> module;
};

class SSAGenerator {
    std::unique_ptr<llvm::LLVMContext> context;
    std::unique_ptr<llvm::Module> module;
    std::unique_ptr<llvm::IRBuilder<>> builder;

    std::unordered_map<const CFG::BasicBlock*, llvm::BasicBlock*> cfgToLLVM{};

    std::unordered_map<std::string_view, std::unordered_map<const CFG::BasicBlock*, llvm::Value*>> currentDef;
    std::unordered_set<const CFG::BasicBlock*> sealed;
    std::unordered_map<const CFG::BasicBlock*, std::unordered_map<std::string_view, llvm::Value*>> incompletePhis;
    void writeVariable(std::string_view var, const CFG::BasicBlock* block, llvm::Value* value);

    void addPhiOperands(std::string_view var, llvm::Value* phi);

    llvm::Value* readVariableRecursive(std::string_view var, const CFG::BasicBlock* block);
    llvm::Value* readVariable(std::string_view var, const CFG::BasicBlock* block);

    llvm::BasicBlock* createNewBasicBlock(llvm::Function* parentFunction, std::string_view name,
                                          const CFG::BasicBlock* correspondingCFGBlock);
    void codegenBlock(llvm::BasicBlock* curr, const CFG::BasicBlock* currCFG, llvm::Function* function);
    llvm::Value* codegenExpression(llvm::BasicBlock* curr, const AST::Expression& expr);
    void codegenStatementSeq(llvm::BasicBlock* curr, const CFG::BasicBlock* currCFG, llvm::Function* function);
    void codegenExprStatement(llvm::BasicBlock*, const AST::Statement& statement);
    void codegenReturnSt(llvm::BasicBlock*, const AST::Expression* returnNode);

  public:
    SSAGenerator()
        : context{{std::make_unique<llvm::LLVMContext>()}}, module{std::make_unique<llvm::Module>("mainMod", *context)},
          builder{std::make_unique<llvm::IRBuilder<>>(*context)} {}

    void codegenFunction(std::string_view name, const CFG::BasicBlock*);
    IntermediateRepresentation extractResult() {
        return IntermediateRepresentation{std::move(context), std::move(module)};
    }
};

inline IntermediateRepresentation generateIR(CFG::CFG& cfg) {
    SSAGenerator ssaGen{};
    for (auto& [name, prelude] : cfg.functions) {
        ssaGen.codegenFunction(name, &prelude);
    }
    return ssaGen.extractResult();
}