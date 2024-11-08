#pragma once
#include "Parser/AST.h"
#include "llvm/IR/Module.h"
#include <memory>

struct IntermediateRepresentation {
    std::unique_ptr<llvm::LLVMContext> context;
    std::unique_ptr<llvm::Module> module;
};

IntermediateRepresentation generateIR(AST::AST ast);