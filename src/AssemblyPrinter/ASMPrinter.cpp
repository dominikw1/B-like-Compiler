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

    std::string turnRegisterIndexIntoString(RegisterSize size, uint8_t index) {
        std::string prefix = [](RegisterSize size) {
            switch (size) {
            case RegisterSize::S_64:
                return "r";
            case RegisterSize::S_32:
                return "e";
            }
            throw std::runtime_error("Unsupported register size");
        }(size);
        assert(index < 16);
        return prefix + regNames[index];
    }

    std::string turnInstruction(llvm::StringRef instrName, llvm::CallInst* call) {
        assert(instructions.contains(std::string(instrName)));
        auto& config = instructions.at(std::string(instrName));
        std::string instr = "\t" + std::string{config.mnemonic};
        bool firstInstr = true;
        for (size_t i = 0, argI = 0; i < config.args.size(); ++i, ++argI) {
            if (firstInstr)
                firstInstr = false;
            else
                instr += ",";

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

                instr += ((cast<llvm::ConstantInt>(call->getArgOperand(argI))->getSExtValue() > 0) ? " + " : " - ") +
                         std::to_string(abs(cast<llvm::ConstantInt>(call->getArgOperand(argI))->getSExtValue()));
                instr += "] ";
            }
        }

        return instr;
    }

    std::string turnBlockToASM(llvm::BasicBlock& block) {
        std::string blockString = std::format(".L{}:\n", labelMap.size());
        labelMap[&block] = labelMap.size();
        for (auto& instr : block) {
            if (dyn_cast<llvm::ReturnInst>(&instr)) {
                blockString += "\tret\n";
                continue;
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
    std::string modStr = ".file \"test.cpp\"\n.intel_syntax noprefix\n.text\n";
    for (auto& func : module) {
        modStr += asmPrinter.turnFunctionToASM(func);
    }
    modStr += "\n.ident  \"Ubuntu clang version 20.0.0 (++20241121082202+6f76b2a3c010-1~exp1~20241121082219.555)\"\n";
    modStr += ".section        \".note.GNU-stack\",\"\",@progbits\n";

    return modStr;
}
