#pragma once
#include "llvm/IR/Module.h"

void allocateRegisters(llvm::Module& module,const llvm::DenseSet<llvm::StringRef>&normalFunctions);