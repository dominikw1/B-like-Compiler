#pragma once
#include "string_view"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/DenseMap.h"
#include <unordered_map>
enum class RegisterSize { S_64, S_32, S_16 };

enum class ArgumentType { Register, Immediate, Memeory };

struct Argument {
    ArgumentType type;
    RegisterSize regSize;
};

struct InstructionInfo {
    std::string_view mnemonic;
    llvm::SmallVector<Argument> args;
};

std::unordered_map<std::string, InstructionInfo> initInstructions();
const std::unordered_map<std::string, InstructionInfo> instructions = initInstructions();
