#include "SSAGeneration.h"
#include "../Parser/AST.h"
#include "llvm/IR/Instructions.h"
#include <vector>

void SSAGenerator::writeVariable(std::string_view var, const llvm::BasicBlock* block, llvm::Value* value) {
    currentDef[var][block] = value;
}

llvm::Value* SSAGenerator::addPhiOperands(std::string_view var, llvm::PHINode* phi, llvm::BasicBlock* block) {
    for (auto* pred : llvm::predecessors(block)) {
        phi->addIncoming(readVariable(var, pred), pred);
    }
    // TODO: reduce phi
    return phi;
}

void SSAGenerator::sealBlock(llvm::BasicBlock* block) {
    for (auto var : incompletePhis[block]) {
        addPhiOperands(var.first, llvm::cast<llvm::PHINode>(var.second), block);
    }
    sealed.insert(block);
}

llvm::Value* SSAGenerator::readVariableRecursive(std::string_view var, llvm::BasicBlock* block) {
    llvm::Value* val{};
    if (!sealed.contains(block)) {
        // Incomplete CFG
        val = llvm::PHINode::Create(llvm::Type::getInt64Ty(*context), 0);
        incompletePhis[block][var] = val;
    } else if (llvm::pred_size(block) == 1) {
        // Optimize the common case of one predecessor : No phi needed
        val = readVariable(var, *llvm::pred_begin(block));
    } else {
        auto* phi = llvm::PHINode::Create(llvm::Type::getInt64Ty(*context), llvm::pred_size(block));
        writeVariable(var, block, phi);
        val = addPhiOperands(var, phi, block);
    }
    writeVariable(var, block, val);
    return val;
}

llvm::Value* SSAGenerator::readVariable(std::string_view var, llvm::BasicBlock* block) {
    if (currentDef[var].contains(block))
        return currentDef[var][block];
    return readVariableRecursive(var, block);
}

llvm::BasicBlock* SSAGenerator::createNewBasicBlock(llvm::Function* parentFunction, std::string_view name,
                                                    const CFG::BasicBlock* correspondingCFGBlock) {
    return llvm::BasicBlock::Create(*context, name, parentFunction);
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
    llvm::Value* leftValBoolean = builder->CreateICmpNE(
        leftVal, llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context), 0, true), "leftValBoolean");
    llvm::Value* branchToFalse = builder->CreateCondBr(leftValBoolean, resultBlock, falseBlock);

    auto* trueBlock = currBlock;
    switchToBlock(falseBlock);
    sealBlock(falseBlock);

    llvm::Value* rightVal = codegenExpression(right, currCFG);
    llvm::Value* rightValBoolean =
        builder->CreateICmpNE(rightVal, llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context), 0));

    llvm::Value* branchToResult = builder->CreateBr(resultBlock);
    switchToBlock(resultBlock);
    sealBlock(resultBlock);

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

llvm::Value* SSAGenerator::codegenPlus(const AST::Expression& left, const AST::Expression& right,
                                       const CFG::BasicBlock* currCFG) {
    auto* leftV = codegenExpression(left, currCFG);
    return builder->CreateAdd(leftV, codegenExpression(right, currCFG), "add");
}

llvm::Value* SSAGenerator::codegenBinaryOp(const AST::Expression& expr, const CFG::BasicBlock* currCFG) {
    auto& binExp = static_cast<const AST::BinaryOperator&>(expr);

    switch (binExp.type) {
    case TokenType::And_Bit:
        return codegenAndBit(*binExp.operand1, *binExp.operand2, currCFG);
    case TokenType::Plus:
        return codegenPlus(*binExp.operand1, *binExp.operand2, currCFG);
    case TokenType::And_Logical:
        return codegenAndLogical(*binExp.operand1, *binExp.operand2, currCFG);
    default:
        throw std::runtime_error("Unimplemented binop for codegen");
    }
}

llvm::Value* SSAGenerator::codegenExpression(const AST::Expression& expr, const CFG::BasicBlock* currCFG) {
    switch (expr.getType()) {
    case AST::ExpressionType::Value:
        return llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context), static_cast<const AST::Value&>(expr).val);
    case AST::ExpressionType::BinaryOperator:
        return codegenBinaryOp(expr, currCFG);
    case AST::ExpressionType::Name:
        return readVariable(static_cast<const AST::Name&>(expr).literal, currBlock);
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
        // std::cout << "Codegening seq" << std::endl;
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
    auto paramNames = extractParameterNamesFromFunction(prelude);
    size_t numParams = paramNames.size();
    std::vector<llvm::Type*> parameters(numParams, llvm::Type::getInt64Ty(*context));
    auto type =
        llvm::FunctionType::get(CFG::doesFunctionHaveNonVoidReturnType(prelude) ? llvm::Type::getInt64Ty(*context)
                                                                                : llvm::Type::getVoidTy(*context),
                                parameters, false);
    llvm::FunctionCallee funcCallee = this->module->getOrInsertFunction(name, type);
    currFunc = dyn_cast<llvm::Function>(funcCallee.getCallee());
    if (prelude->posterior.size() == 1 && prelude->posterior[0]->type == CFG::BlockType::FunctionEpilogue) {
        // empty function
        return;
    }

    auto* entryBlock = createNewBasicBlock(currFunc, "entry", prelude);
    currBlock = entryBlock;

    auto argIt = currFunc->arg_begin();
    for (size_t i = 0; i < numParams; ++i) {
        writeVariable(paramNames.at(i), currBlock, argIt);
        (argIt++)->setName(paramNames.at(i));
    }

    builder->SetInsertPoint(currBlock);

    codegenBlock(prelude);
    sealBlock(entryBlock);

    // add empty ret if it is not there already
    if (currBlock->size() != 0) {
        if (!currBlock->rbegin()->isTerminator()) {
            builder->CreateRetVoid();
        }
    }

    if (llvm::verifyFunction(*currFunc, &llvm::errs())) {
        throw std::runtime_error("Invalid function");
    }
}