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
#include <map>
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
    llvm::BasicBlock* currBlock; // raw pointer as we do not explicitly manage the memory
    llvm::Function* currFunc;

    std::unordered_map<std::string_view, std::unordered_map<const llvm::BasicBlock*, llvm::Value*>> currentDef;
    std::unordered_set<const llvm::BasicBlock*> sealed;
    std::unordered_map<const llvm::BasicBlock*, std::unordered_map<std::string_view, llvm::Value*>> incompletePhis;

    void writeVariable(std::string_view var, const llvm::BasicBlock* block, llvm::Value* value);

    llvm::Value* addPhiOperands(std::string_view var, llvm::PHINode* phi, llvm::BasicBlock* block);
    void sealBlock(llvm::BasicBlock* block);
    llvm::Value* readVariableRecursive(std::string_view var, llvm::BasicBlock* block);
    llvm::Value* readVariable(std::string_view var, llvm::BasicBlock* block);
    llvm::Value* readFromPtrIfAlloca(llvm::Value* v);
    llvm::Value* reduceTrivialPhi(llvm::PHINode* phi);

    llvm::BasicBlock* createNewBasicBlock(llvm::Function* parentFunction, std::string_view name);
    void switchToBlock(llvm::BasicBlock* newBlock);
    void codegenBlock(const CFG::BasicBlock* currCFG);
    llvm::Value* codegenExpression(const AST::Expression& expr);
    void codegenStatementSeq(const CFG::BasicBlock* currCFG);
    void codegenExprStatement(const AST::ExpressionStatement& statement);
    void codegenReturnSt(const AST::Expression* returnNode);
    void codegenAssignment(const AST::Assignment& assignmentStatement);
    void codegenIf(const CFG::BasicBlock* ifBlock);

    llvm::Value* codegenBinaryOp(const AST::Expression& expr);
    llvm::Value* codegenUnaryOp(const AST::Expression& expr);
    llvm::Value* codegenAndLogical(const AST::Expression& left, const AST::Expression& right);
    llvm::Value* codegenAndBit(const AST::Expression& left, const AST::Expression& right);

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
