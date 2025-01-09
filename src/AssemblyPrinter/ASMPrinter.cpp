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
        {"ax", "cx", "dx", "bx" /*?*/, "sp", "fp", "si", "di", "8", "9", "10", "11", "12", "13", "14", "15"}};

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
        std::string instr = std::string{config.mnemonic};
        for (size_t i = 0, argI = 0; i < config.args.size(); ++i, ++argI) {
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
                instr += " [" + turnRegisterIndexIntoString(
                                    config.args[i].regSize,
                                    cast<llvm::ConstantInt>(call->getArgOperand(argI++))->getSExtValue());
                instr += " + " + std::to_string(cast<llvm::ConstantInt>(call->getArgOperand(argI++))->getSExtValue());
                instr += " * " + turnRegisterIndexIntoString(
                                     config.args[i].regSize,
                                     cast<llvm::ConstantInt>(call->getArgOperand(argI++))->getSExtValue());

                instr += " + " + std::to_string(cast<llvm::ConstantInt>(call->getArgOperand(argI))->getSExtValue());
                instr += "] ";
            }
        }

        return instr;
    }

    std::string turnBlockToASM(llvm::BasicBlock& block) {
        std::string blockString;
        for (auto& instr : block) {
            if (dyn_cast<llvm::ReturnInst>(&instr)) {
                blockString += "ret\n";
                continue;
            }
            if (auto* call = dyn_cast<llvm::CallInst>(&instr)) {
                if (call->getCalledFunction()->getName() == "R_FRAME_SETUP") {
                    blockString += "push rbp\n";
                    blockString += "mov rbp, rsp\n";
                    assert(llvm::dyn_cast<llvm::ConstantInt>(call->getArgOperand(0)));
                    blockString += std::format("sub esp {}\n",
                                               llvm::cast<llvm::ConstantInt>(call->getArgOperand(0))->getSExtValue());
                    continue;
                }
                if (call->getCalledFunction()->getName() == "R_FRAME_DESTROY") {
                    blockString += "leave\n";
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
        std::string funcStr = "";
        for (auto& bb : function) {
            funcStr += turnBlockToASM(bb);
        }
        return funcStr;
    }
};
} // namespace

std::string turnToASM(llvm::Module& module) {
    ASMPrinter asmPrinter{};
    std::string modStr = "";
    for (auto& func : module) {
        modStr += asmPrinter.turnFunctionToASM(func);
    }
    return modStr;
}
