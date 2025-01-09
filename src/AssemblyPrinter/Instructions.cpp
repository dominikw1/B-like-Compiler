#include "Instructions.h"

std::unordered_map<std::string, InstructionInfo> initInstructions() {
    std::unordered_map<std::string, InstructionInfo> instrs;

    instrs["R_MOV64ri"] = InstructionInfo{
        .mnemonic = "mov",
        .args = {{ArgumentType::Register, RegisterSize::S_64}, {ArgumentType::Immediate, RegisterSize::S_64}}};
    instrs["R_MOV64rr"] = InstructionInfo{
        .mnemonic = "mov",
        .args = {{ArgumentType::Register, RegisterSize::S_64}, {ArgumentType::Register, RegisterSize::S_64}}};
    instrs["R_MOV64mr"] = InstructionInfo{
        .mnemonic = "mov",
        .args = {{ArgumentType::Memeory, RegisterSize::S_64}, {ArgumentType::Register, RegisterSize::S_64}}};
    instrs["R_MOV64rm"] = InstructionInfo{
        .mnemonic = "mov",
        .args = {{ArgumentType::Register, RegisterSize::S_64}, {ArgumentType::Memeory, RegisterSize::S_64}}};
    instrs["R_LEA64rm"] = InstructionInfo{
        .mnemonic = "lea",
        .args = {{ArgumentType::Register, RegisterSize::S_64}, {ArgumentType::Memeory, RegisterSize::S_64}}};
    return instrs;
}
