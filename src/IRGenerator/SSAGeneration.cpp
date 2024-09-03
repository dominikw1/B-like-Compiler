#include "SSAGeneration.h"
#include "../Parser/AST.h"
#include <vector>

void SSAGenerator::writeVariable(std::string_view var, const CFG::BasicBlock* block, llvm::Value* value) {
    currentDef[var][block] = value;
}

void SSAGenerator::addPhiOperands(std::string_view var, llvm::Value* phi) {
    // for (auto* pred : phi.block->predecessors) {
    // phi.operands.push_back(readVariable(var, pred));
    //}
}

llvm::Value* SSAGenerator::readVariableRecursive(std::string_view var, const CFG::BasicBlock* block) {
    llvm::Value* val{};
    if (!sealed.contains(block)) {
        // Incomplete CFG
        // val = {.block = block};
        incompletePhis[block][var] = val;
    } else if (block->predecessors.size() == 1) {
        // Optimize the common case of one predecessor : No phi needed
        val = readVariable(var, block->predecessors[0]);
    } else {
        // val = {.block = block};
        writeVariable(var, block, val);
        //  addPhiOperands(variable, val)
    }
}

llvm::Value* SSAGenerator::readVariable(std::string_view var, const CFG::BasicBlock* block) {
    if (currentDef[var].contains(block))
        return currentDef[var][block];
    return readVariableRecursive(var, block);
}

llvm::BasicBlock* SSAGenerator::createNewBasicBlock(llvm::Function* parentFunction, std::string_view name,
                                                    const CFG::BasicBlock* correspondingCFGBlock) {
    auto* newBlock = llvm::BasicBlock::Create(*context, name, parentFunction);
    cfgToLLVM[correspondingCFGBlock] = newBlock;
    return newBlock;
}

void SSAGenerator::codegenBlock(llvm::BasicBlock curr, const CFG::BasicBlock* currCFG, llvm::Function* function) {}

void SSAGenerator::codegenFunction(std::string_view name, const CFG::BasicBlock* prelude) {
    auto* params = static_cast<const CommaList*>(prelude->extraInfo[0]);
    auto paramNames = params->getAllNamesOnTopLevel();
    size_t numParams = prelude->extraInfo.at(0) == nullptr ? 0 : params->getNumInList();
    std::vector<llvm::Type*> parameters(numParams, llvm::Type::getInt64Ty(*context));
    auto type =
        llvm::FunctionType::get(CFG::doesFunctionHaveNonVoidReturnType(prelude) ? llvm::Type::getInt64Ty(*context)
                                                                                : llvm::Type::getVoidTy(*context),
                                parameters, false);
    llvm::FunctionCallee funcCallee = this->module->getOrInsertFunction(name, type);
    llvm::Function* func = dyn_cast<llvm::Function>(funcCallee.getCallee());

    auto argIt = func->arg_begin();
    for (size_t i = 0; i < numParams; ++i) {
        (argIt++)->setName(paramNames[i]);
    }

    llvm::BasicBlock* entry = createNewBasicBlock(func, "entry", prelude);
}