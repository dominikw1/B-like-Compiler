#pragma once
#include <string>
#include <llvm/IR/Module.h>

std::string turnToASM(llvm::Module& module); 