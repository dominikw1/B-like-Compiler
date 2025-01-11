#include "ASMPrinter.h"
#include "Instructions.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Type.h"
#include <format>
namespace {

class ASMPrinter {
    llvm::DenseMap<llvm::BasicBlock*, uint64_t> labelMap;

    constexpr static const std::array<std::string, 16> regNames{
        {"ax", "cx", "dx", "bx" /*?*/, "sp", "bp", "si", "di", "8", "9", "10", "11", "12", "13", "14", "15"}};

    constexpr static const std::array<std::string, 16> regNames8Bit{"al",   "cl",   "dl",   "bl",  "spl",  "bpl",
                                                                    "sil",  "dil",  "r8b",  "r9b", "r10b", "r11b",
                                                                    "r12b", "r13b", "r14b", "r15b"};

    std::string turnRegisterIndexIntoString(RegisterSize size, uint8_t index) {
        if (size == RegisterSize::S_8) {
            return regNames8Bit.at(index);
        }
        std::string prefix = [](RegisterSize size) {
            switch (size) {
            case RegisterSize::S_64:
                return "r";
            case RegisterSize::S_32:
                return "e";
            case RegisterSize::S_16:
                return "";
            }
            throw std::runtime_error("Unsupported register size");
        }(size);
        assert(index < 16);
        if (index <= 8)
            return prefix + regNames[index];
        else {
            switch (size) {
            case RegisterSize::S_64:
                return std::format("r{}", regNames[index]);
            case RegisterSize::S_32:
                return std::format("r{}d", regNames[index]);
            case RegisterSize::S_16:
                return std::format("r{}w", regNames[index]);
            }
            __builtin_unreachable();
        }
    }

    constexpr static auto specialInstructions = {"R_SETcc8r"};

    std::string handleSpecialInst(llvm::StringRef instrName, llvm::CallInst* call) {
        if (instrName == "R_SETcc8r") {
            auto mnemonic = [](uint32_t byte) {
                switch (byte) {
                case 4:
                    return "SETE";
                case 5:
                    return "SETNE";
                case 12:
                    return "SETNGE";
                case 0xe:
                    return "SETNG";
                case 0xd:
                    return "SETNL";
                case 15:
                    return "SETNLE";
                default:
                    throw std::runtime_error(std::format("Unknown SET opcode {}", byte));
                }
            }(cast<llvm::ConstantInt>(call->getArgOperand(1))->getSExtValue());
            auto registerMnem = regNames8Bit.at(cast<llvm::ConstantInt>(call->getArgOperand(0))->getSExtValue());
            return std::format("{} {}\n", mnemonic, registerMnem);
        }
        throw std::runtime_error("Unknown special instruction");
    }

    std::string turnInstruction(llvm::StringRef instrName, llvm::CallInst* call) {
        if (std::find(specialInstructions.begin(), specialInstructions.end(), instrName) != specialInstructions.end()) {
            return handleSpecialInst(instrName, call);
        }
        assert(instructions.contains(std::string(instrName)));
        auto& config = instructions.at(std::string(instrName));
        std::string instr = "\t" + std::string{config.mnemonic};
        bool firstInstr = true;
        for (size_t i = 0, argI = 0; i < config.args.size(); ++i, ++argI) {
            if (firstInstr)
                firstInstr = false;
            else
                instr += ",";
            if (config.args[i].prefix) {
                instr += std::format(" {} ", config.args[i].prefix.value());
            }
            switch (config.args[i].type) {
            case ArgumentType::Register:
                instr += " " + turnRegisterIndexIntoString(
                                   config.args[i].regSize,
                                   cast<llvm::ConstantInt>(call->getArgOperand(argI))->getSExtValue());
                break;
            case ArgumentType::Immediate:
                instr += " " + std::to_string(cast<llvm::ConstantInt>(call->getArgOperand(argI))->getSExtValue());
                break;
            case ArgumentType::Memeory:
                //[base register + constant offset + offset register * constant size]

                instr += " [" + turnRegisterIndexIntoString(
                                    config.args[i].regSize,
                                    cast<llvm::ConstantInt>(call->getArgOperand(argI++))->getSExtValue());
                instr += " + " + std::to_string(cast<llvm::ConstantInt>(call->getArgOperand(argI++))->getSExtValue());
                instr += " * " + turnRegisterIndexIntoString(
                                     config.args[i].regSize,
                                     cast<llvm::ConstantInt>(call->getArgOperand(argI++))->getSExtValue());
                if (cast<llvm::ConstantInt>(call->getArgOperand(argI))->getSExtValue() != 0) {
                    instr +=
                        ((cast<llvm::ConstantInt>(call->getArgOperand(argI))->getSExtValue() > 0) ? " + " : " - ") +
                        std::to_string(abs(cast<llvm::ConstantInt>(call->getArgOperand(argI))->getSExtValue()));
                }
                instr += "] ";
            }
        }

        return instr;
    }

    std::string turnBlockToASM(llvm::BasicBlock& block) {
        std::string blockString = std::format(".L{}:\n", labelMap.at(&block));

        for (auto& instr : block) {
            if (dyn_cast<llvm::ReturnInst>(&instr)) {
                blockString += "\tret\n";
                continue;
            }

            if (auto* br = dyn_cast<llvm::BranchInst>(&instr)) {
                if (br->isUnconditional()) {
                    blockString += std::format("\tjmp .L{}", labelMap.at(br->getSuccessor(0)));
                } else {
                    assert(br->getNumSuccessors() == 2);
                    // ignore, we handled in jcc
                    continue;
                }
            }

            if (auto* call = dyn_cast<llvm::CallInst>(&instr)) {
                if (call->getCalledFunction()->getName() == "R_FRAME_SETUP") {
                    blockString += "\tpush rbp\n";
                    blockString += "\tmov rbp, rsp\n";
                    assert(llvm::dyn_cast<llvm::ConstantInt>(call->getArgOperand(0)));
                    blockString += std::format("\tsub esp, {}\n",
                                               llvm::cast<llvm::ConstantInt>(call->getArgOperand(0))->getSExtValue());
                    continue;
                }
                if (call->getCalledFunction()->getName() == "R_FRAME_DESTROY") {
                    blockString += "\tleave\n";
                    continue;
                }

                // normal case
                blockString += turnInstruction(call->getCalledFunction()->getName(), call) + "\n";
            }
        }
        return blockString;
    }

  public:
    std::string turnFunctionToASM(llvm::Function& function) {
        if (function.isDeclaration())
            return "";
        for (auto& bb : function) {
            labelMap[&bb] = labelMap.size();
        }

        std::string_view funcName = function.getName();
        std::string funcStr =
            std::format(".globl {}\n.p2align 4\n.type {}, @function\n{}:\n", funcName, funcName, funcName);
        for (auto& bb : function) {
            funcStr += turnBlockToASM(bb);
        }
        funcStr += std::format(".size   {}, .-{}", funcName, funcName);
        return funcStr;
    }
};
} // namespace

std::string turnToASM(llvm::Module& module) {
    ASMPrinter asmPrinter{};
    std::string modStr = ".file \"42.b\"\n.intel_syntax noprefix\n.text\n";
    for (auto& func : module) {
        modStr += asmPrinter.turnFunctionToASM(func);
    }
    modStr += "\n.ident  \"My compiler :)\"\n";
    modStr += ".section        \".note.GNU-stack\",\"\",@progbits\n";

    return modStr;
}
