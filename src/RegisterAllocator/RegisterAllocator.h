#pragma once
#include "llvm/IR/Module.h"

void allocateRegisters(llvm::Module& module, llvm::DenseSet<llvm::StringRef>& normalFunctions);