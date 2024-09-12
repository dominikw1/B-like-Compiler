#include "SSAGeneration.h"
#include "../Parser/AST.h"
#include "llvm/IR/Instructions.h"
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
    // hack for debug reasons
    return (*currentDef[var].begin()).second;
    return readVariableRecursive(var, block);
}

llvm::BasicBlock* SSAGenerator::createNewBasicBlock(llvm::Function* parentFunction, std::string_view name,
                                                    const CFG::BasicBlock* correspondingCFGBlock) {
    auto* newBlock = llvm::BasicBlock::Create(*context, name, parentFunction);
    cfgToLLVM[correspondingCFGBlock] = newBlock;
    return newBlock;
}

void SSAGenerator::switchToBlock(llvm::BasicBlock* newBlock) {
    currBlock = newBlock;
    builder->SetInsertPoint(currBlock);
}

llvm::Value* SSAGenerator::codegenAndLogical(const AST::Expression& left, const AST::Expression& right,
                                             const CFG::BasicBlock* currCFG) {
    auto* falseBlock = createNewBasicBlock(currFunc, "noShortCircuit", currCFG);
    auto* resultBlock = createNewBasicBlock(currFunc, "boolExprResult", currCFG);

    llvm::Value* leftVal = codegenExpression(left, currCFG);
    std::cout << "left Val: ";
    leftVal->dump();
    llvm::Value* leftValBoolean = builder->CreateICmpNE(
        leftVal, llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context), 0, true), "leftValBoolean");
    std::cout << "\nleft ValBool: ";
    leftValBoolean->dump();
    std::cout << std::endl;
    llvm::Value* branchToFalse = builder->CreateCondBr(leftValBoolean, resultBlock, falseBlock);

    auto* trueBlock = currBlock;
    switchToBlock(falseBlock);

    llvm::Value* rightVal = codegenExpression(right, currCFG);
    llvm::Value* rightValBoolean =
        builder->CreateICmpNE(rightVal, llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context), 0));

    llvm::Value* branchToResult = builder->CreateBr(resultBlock);
    switchToBlock(resultBlock);

    auto exprResult = llvm::PHINode::Create(llvm::Type::getInt1Ty(*context), 2, "boolExprResultNode");
    exprResult->insertInto(resultBlock, resultBlock->begin());
    exprResult->addIncoming(llvm::ConstantInt::get(llvm::Type::getInt1Ty(*context), 1), trueBlock);
    exprResult->addIncoming(rightValBoolean, falseBlock);
    return exprResult;
}

llvm::Value* SSAGenerator::codegenAndBit(const AST::Expression& left, const AST::Expression& right,
                                         const CFG::BasicBlock* currCFG) {
    auto* leftV = codegenExpression(left, currCFG);
    // structured this way to enforce evaluation of left before right in case we change state
    return builder->CreateAnd(leftV, codegenExpression(right, currCFG));
}

llvm::Value* SSAGenerator::codegenBinaryOp(const AST::Expression& expr, const CFG::BasicBlock* currCFG) {
    auto& binExp = static_cast<const AST::BinaryOperator&>(expr);

    switch (binExp.type) {
    case TokenType::And_Bit:
        return codegenAndBit(*binExp.operand1, *binExp.operand2, currCFG);
    case TokenType::And_Logical:
        return codegenAndLogical(*binExp.operand1, *binExp.operand2, currCFG);
    default:
        throw std::runtime_error("Unimplemented binop for codegen");
    }
}

llvm::Value* SSAGenerator::codegenExpression(const AST::Expression& expr, const CFG::BasicBlock* currCFG) {
    std::cout << "in codegen expr" << std::endl;
    switch (expr.getType()) {
    case AST::ExpressionType::Value:
        return llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context), static_cast<const AST::Value&>(expr).val);
    case AST::ExpressionType::BinaryOperator:
        return codegenBinaryOp(expr, currCFG);
    case AST::ExpressionType::Name:
        return readVariable(static_cast<const AST::Name&>(expr).literal, currCFG);
    default:
        throw std::runtime_error(std::format("Not implemented expr {}", expr.toString()));
    }
}
void SSAGenerator::codegenExprStatement(const AST::Statement& statement, const CFG::BasicBlock* currCFG) {
    codegenExpression(*((static_cast<const AST::ExpressionStatement&>(statement)).expression), currCFG);
}

void SSAGenerator::codegenStatementSeq(const CFG::BasicBlock* currCFG) {
    builder->SetInsertPoint(currBlock);
    for (auto& statement : currCFG->extraInfo) {

        switch (statement->getType()) {
        case AST::ExpressionType::ExpressionStatement:
            codegenExprStatement(static_cast<const AST::Statement&>(*statement), currCFG);
            break;
        default:
            throw std::runtime_error("Not implemented");
        }
    }
}

void SSAGenerator::codegenReturnSt(const AST::Expression* ret, const CFG::BasicBlock* currCFG) {
    if (ret) {
        builder->CreateRet(codegenExpression(*ret, currCFG));
    } else {
        builder->CreateRetVoid();
    }
}

void SSAGenerator::codegenBlock(const CFG::BasicBlock* currCFG) {
    builder->SetInsertPoint(currBlock);
    switch (currCFG->type) {
    case CFG::BlockType::FunctionPrologue:
        // params dealt with in parent function
        codegenBlock(currCFG->posterior.at(0).get());
        break;
    case CFG::BlockType::Normal:
        std::cout << "Codegening seq" << std::endl;
        codegenStatementSeq(currCFG);
        codegenBlock(currCFG->posterior.at(0).get());
        break;
    case CFG::BlockType::FunctionEpilogue:
        break;
    case CFG::BlockType::Return:
        codegenReturnSt(currCFG->extraInfo.at(0), currCFG);
        break;
    default:
        throw std::runtime_error("Not implemented block type");
    }
}

std::vector<std::string_view> extractParameterNamesFromFunction(const CFG::BasicBlock* prelude) {
    auto paramNode = prelude->extraInfo[0];
    if (paramNode == nullptr)
        return {};
    // if its not a nullptr it is a parenthesised
    paramNode = static_cast<const AST::Parenthesised*>(paramNode)->inner.get();
    if (paramNode->getType() == AST::ExpressionType::Name) {
        return {static_cast<const AST::Name*>(paramNode)->literal};
    }
    auto* params = static_cast<const AST::CommaList*>(paramNode);
    return params->getAllNamesOnTopLevel();
}

void SSAGenerator::codegenFunction(std::string_view name, const CFG::BasicBlock* prelude) {
    // std::cout << prelude->extraInfo[0]->toString();
    auto paramNames = extractParameterNamesFromFunction(prelude);
    size_t numParams = paramNames.size();
    std::vector<llvm::Type*> parameters(numParams, llvm::Type::getInt64Ty(*context));
    auto type =
        llvm::FunctionType::get(CFG::doesFunctionHaveNonVoidReturnType(prelude) ? llvm::Type::getInt64Ty(*context)
                                                                                : llvm::Type::getVoidTy(*context),
                                parameters, false);
    llvm::FunctionCallee funcCallee = this->module->getOrInsertFunction(name, type);
    currFunc = dyn_cast<llvm::Function>(funcCallee.getCallee());

    auto argIt = currFunc->arg_begin();
    for (size_t i = 0; i < numParams; ++i) {
        writeVariable(paramNames.at(i), prelude, argIt);
        (argIt++)->setName(paramNames.at(i));
    }

    currBlock = createNewBasicBlock(currFunc, "entry", prelude);

    builder->SetInsertPoint(currBlock);

    codegenBlock(prelude);

    llvm::verifyFunction(*currFunc);
}