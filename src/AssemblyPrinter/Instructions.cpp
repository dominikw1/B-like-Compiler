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
    instrs["R_MOV64mi"] = InstructionInfo{.mnemonic = "mov",
                                          .args = {{ArgumentType::Memeory, RegisterSize::S_64, "QWORD PTR"},
                                                   {ArgumentType::Immediate, RegisterSize::S_64}}};
    instrs["R_MOV16mi"] = InstructionInfo{.mnemonic = "mov",
                                          .args = {{ArgumentType::Memeory, RegisterSize::S_64, "WORD PTR"},
                                                   {ArgumentType::Immediate, RegisterSize::S_64}}};
    instrs["R_MOV8mi"] = InstructionInfo{.mnemonic = "mov",
                                         .args = {{ArgumentType::Memeory, RegisterSize::S_64, "BYTE PTR"},
                                                  {ArgumentType::Immediate, RegisterSize::S_64}}};
    instrs["R_LEA64rm"] = InstructionInfo{
        .mnemonic = "lea",
        .args = {{ArgumentType::Register, RegisterSize::S_64}, {ArgumentType::Memeory, RegisterSize::S_64}}};
    instrs["R_SUB64rr"] = InstructionInfo{
        .mnemonic = "sub",
        .args = {{ArgumentType::Register, RegisterSize::S_64}, {ArgumentType::Register, RegisterSize::S_64}}};
    instrs["R_SUB64ri"] = InstructionInfo{
        .mnemonic = "sub",
        .args = {{ArgumentType::Register, RegisterSize::S_64}, {ArgumentType::Immediate, RegisterSize::S_64}}};
    instrs["R_ADD64rr"] = InstructionInfo{
        .mnemonic = "add",
        .args = {{ArgumentType::Register, RegisterSize::S_64}, {ArgumentType::Register, RegisterSize::S_64}}};
    instrs["R_ADD64ri"] = InstructionInfo{
        .mnemonic = "add",
        .args = {{ArgumentType::Register, RegisterSize::S_64}, {ArgumentType::Immediate, RegisterSize::S_64}}};
    instrs["R_XOR64ri"] = InstructionInfo{
        .mnemonic = "xor",
        .args = {{ArgumentType::Register, RegisterSize::S_64}, {ArgumentType::Immediate, RegisterSize::S_64}}};
    instrs["R_XOR64rr"] = InstructionInfo{
        .mnemonic = "xor",
        .args = {{ArgumentType::Register, RegisterSize::S_64}, {ArgumentType::Register, RegisterSize::S_64}}};
    instrs["R_CMP64ri"] = InstructionInfo{
        .mnemonic = "cmp",
        .args = {{ArgumentType::Register, RegisterSize::S_64}, {ArgumentType::Immediate, RegisterSize::S_64}}};
    instrs["R_CMP64rr"] = InstructionInfo{
        .mnemonic = "cmp",
        .args = {{ArgumentType::Register, RegisterSize::S_64}, {ArgumentType::Register, RegisterSize::S_64}}};

    instrs["R_AND64ri"] = InstructionInfo{
        .mnemonic = "and",
        .args = {{ArgumentType::Register, RegisterSize::S_64}, {ArgumentType::Immediate, RegisterSize::S_64}}};

    instrs["R_AND64rr"] = InstructionInfo{
        .mnemonic = "and",
        .args = {{ArgumentType::Register, RegisterSize::S_64}, {ArgumentType::Register, RegisterSize::S_64}}};
    instrs["R_MOVZXB32rm"] = InstructionInfo{.mnemonic = "movzx",
                                             .args = {{ArgumentType::Register, RegisterSize::S_64},
                                                      {ArgumentType::Memeory, RegisterSize::S_64, "BYTE PTR"}}};
    instrs["R_MOVZXW32rm"] = InstructionInfo{.mnemonic = "movzx",
                                             .args = {{ArgumentType::Register, RegisterSize::S_64},
                                                      {ArgumentType::Memeory, RegisterSize::S_64, "WORD PTR"}}};
    instrs["R_MOVZXB32rr"] = InstructionInfo{
        .mnemonic = "movzx",
        .args = {{ArgumentType::Register, RegisterSize::S_64}, {ArgumentType::Register, RegisterSize::S_8}}};
    instrs["R_MOVZXW32rr"] = InstructionInfo{
        .mnemonic = "movzx",
        .args = {{ArgumentType::Register, RegisterSize::S_64}, {ArgumentType::Register, RegisterSize::S_16}}};
    
    instrs["R_MOVSXB64rr"] = InstructionInfo{
        .mnemonic = "movsx",
        .args = {{ArgumentType::Register, RegisterSize::S_64}, {ArgumentType::Register, RegisterSize::S_8}}};
  
    instrs["R_MOVSXB64rm"] = InstructionInfo{
        .mnemonic = "movsx",
        .args = {{ArgumentType::Register, RegisterSize::S_64}, {ArgumentType::Memeory, RegisterSize::S_64, "BYTE PTR"}}};
    
    
    instrs["R_MOVSXW64rr"] = InstructionInfo{
        .mnemonic = "movsx",
        .args = {{ArgumentType::Register, RegisterSize::S_64}, {ArgumentType::Register, RegisterSize::S_16}}};
  
    instrs["R_MOVSXW64rm"] = InstructionInfo{
        .mnemonic = "movsx",
        .args = {{ArgumentType::Register, RegisterSize::S_64}, {ArgumentType::Memeory, RegisterSize::S_64, "WORD PTR"}}};
    
    
    instrs["R_MOVSXWD64rr"] = InstructionInfo{
        .mnemonic = "movsx",
        .args = {{ArgumentType::Register, RegisterSize::S_64}, {ArgumentType::Register, RegisterSize::S_32}}};
  
    instrs["R_MOVSXWD64rm"] = InstructionInfo{
        .mnemonic = "movsx",
        .args = {{ArgumentType::Register, RegisterSize::S_64}, {ArgumentType::Memeory, RegisterSize::S_64, "DWORD PTR"}}};
    

  
    return instrs;
}
