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

llvm::Value* SSAGenerator::codegenExpression(llvm::BasicBlock* curr, const AST::Expression& expr) {
    switch (expr.getType()) {
    case AST::ExpressionType::Value:
        return llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context), static_cast<const AST::Value&>(expr).val);
    default:
        throw std::runtime_error("Not implemented");
    }
}
void SSAGenerator::codegenExprStatement(llvm::BasicBlock*, const AST::Statement& statement) {
    // value = codegenExpression(curr, *((static_cast<const AST::ExpressionStatement&>(*statement)).expression));
    // value = builder->CreateAdd(value, value, "add");
    //   cast<llvm::Instruction>(value)->insertInto(curr, curr->end());
}

void SSAGenerator::codegenStatementSeq(llvm::BasicBlock* curr, const CFG::BasicBlock* currCFG,
                                       llvm::Function* function) {
    builder->SetInsertPoint(curr);
    for (auto& statement : currCFG->extraInfo) {

        switch (statement->getType()) {
        case AST::ExpressionType::ExpressionStatement:
            // codegenExprStatement(curr, static_cast<const AST::Statement&>(*statement));
            break;
        // case AST::ExpressionType::Return:
        //   break;
        default:
            throw std::runtime_error("Not implemented");
        }
    }
}

void SSAGenerator::codegenReturnSt(llvm::BasicBlock* curr, const AST::Expression* ret) {
    if (ret) {
        builder->CreateRet(codegenExpression(curr, *ret));
    } else {
        builder->CreateRetVoid();
    }
}

void SSAGenerator::codegenBlock(llvm::BasicBlock* curr, const CFG::BasicBlock* currCFG, llvm::Function* function) {
    builder->SetInsertPoint(curr);
    switch (currCFG->type) {
    case CFG::BlockType::FunctionPrologue:
        // params dealt with in parent function
        codegenBlock(curr, currCFG->posterior.at(0).get(), function);
        break;
    case CFG::BlockType::Normal:
        std::cout << "Codegening seq" << std::endl;
        codegenStatementSeq(curr, currCFG, function);
        codegenBlock(curr, currCFG->posterior.at(0).get(), function);
        break;
    case CFG::BlockType::FunctionEpilogue:
        break;
    case CFG::BlockType::Return:
        codegenReturnSt(curr, currCFG->extraInfo.at(0));
        break;
    default:
        throw std::runtime_error("Not implemented block type");
    }
}

void SSAGenerator::codegenFunction(std::string_view name, const CFG::BasicBlock* prelude) {
    auto* params = static_cast<const AST::CommaList*>(prelude->extraInfo[0]);
    auto paramNames = params ? params->getAllNamesOnTopLevel() : std::vector<std::string_view>{};
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

    builder->SetInsertPoint(entry);
    // builder->CreateRet(llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context), 0, true));

    codegenBlock(entry, prelude, func);

    llvm::verifyFunction(*func);
}