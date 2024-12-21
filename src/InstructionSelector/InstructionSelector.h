#pragma once
#include <llvm/ADT/DenseSet.h>
#include <llvm/IR/Function.h>

llvm::DenseSet<llvm::StringRef> doInstructionSelection(llvm::Module& module);