#include "SSAGeneration.h"
#include "../Parser/AST.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include <vector>

void SSAGenerator::writeVariable(std::string_view var, const llvm::BasicBlock* block, llvm::Value* value) {
    currentDef[var][block] = value;
}

llvm::Value* SSAGenerator::reduceTrivialPhi(llvm::PHINode* phi) {
    llvm::Value* uniqueVal = phi->hasConstantValue();
    if (!uniqueVal) {
        return phi;
    }

    std::vector<llvm::Value*> users;
    for (auto* user : phi->users()) {
        if (user != phi)
            users.push_back(user);
    }
    auto phiIt = phi->getIterator();
    llvm::ReplaceInstWithValue(phiIt, uniqueVal);
    for (auto* user : users) {
        if (user && llvm::isa<llvm::PHINode>(user)) {
            reduceTrivialPhi(llvm::cast<llvm::PHINode>(user));
        }
    }
    return uniqueVal;
}

llvm::Value* SSAGenerator::addPhiOperands(std::string_view var, llvm::PHINode* phi, llvm::BasicBlock* block) {
    for (auto* pred : llvm::predecessors(block)) {
        phi->addIncoming(readVariable(var, pred), pred);
    }
    return reduceTrivialPhi(phi);
}

void SSAGenerator::sealBlock(llvm::BasicBlock* block) {
    for (auto var : incompletePhis[block]) {
        addPhiOperands(var.first, llvm::cast<llvm::PHINode>(var.second), block);
    }
    sealed.insert(block);
}

llvm::Value* SSAGenerator::readFromPtrIfAlloca(llvm::Value* v) {
    if (llvm::isa<llvm::AllocaInst>(v)) {
        return builder->CreateLoad(llvm::Type::getInt64Ty(*context), v);
    }
    return v;
}

llvm::Value* SSAGenerator::readVariableRecursive(std::string_view var, llvm::BasicBlock* block) {
    llvm::Value* val{};
    std::cout << "reading recursively" << var << std::endl;
    if (!sealed.contains(block)) {
        std::cout << "incomplete" << std::endl;
        // Incomplete CFG
        val = llvm::PHINode::Create(llvm::Type::getInt64Ty(*context), 0, "incompletePhi");
        auto* insertPlace = block->getFirstNonPHI();
        if (insertPlace) {
            llvm::cast<llvm::Instruction>(val)->insertBefore(insertPlace);
        } else {
            builder->SetInsertPoint(block);
            builder->Insert(val);
            builder->SetInsertPoint(currBlock);
        }
        incompletePhis[block][var] = val;
        std::cout << "Added proxy phi in block for " << var << std::endl;
        currBlock->dump();
    } else if (llvm::pred_size(block) == 1) {
        std::cout << "one pred" << std::endl;
        // Optimize the common case of one predecessor : No phi needed
        val = readVariable(var, *llvm::pred_begin(block));
    } else {
        std::cout << "new phi" << std::endl;
        auto* phi = llvm::PHINode::Create(llvm::Type::getInt64Ty(*context), llvm::pred_size(block));
        auto* insertPlace = block->getFirstNonPHI();
        if (insertPlace) {
            std::cout << "There are instrs here" << std::endl;
            phi->insertBefore(insertPlace);
        } else {
            std::cout << "First instr in block" << std::endl;
            builder->SetInsertPoint(block);
            builder->Insert(phi);
            builder->SetInsertPoint(currBlock);
        }
        std::cout << "now wriitng var" << std::endl;
        writeVariable(var, block, phi);
        std::cout << "adding phi operands" << std::endl;
        val = addPhiOperands(var, phi, block);
        std::cout << "done";
    }
    writeVariable(var, block, val);
    return val;
}

llvm::Value* SSAGenerator::readVariable(std::string_view var, llvm::BasicBlock* block) {
    std::cout << "reading " << var << std::endl;
    if (currentDef[var].contains(block)) {
        return currentDef[var][block];
    }
    return readVariableRecursive(var, block);
}

llvm::BasicBlock* SSAGenerator::createNewBasicBlock(llvm::Function* parentFunction, std::string_view name) {
    return llvm::BasicBlock::Create(*context, name, parentFunction);
}

void SSAGenerator::switchToBlock(llvm::BasicBlock* newBlock) {
    currBlock = newBlock;
    builder->SetInsertPoint(currBlock);
}

llvm::Value* SSAGenerator::codegenAndLogical(const AST::Expression& left, const AST::Expression& right) {
    auto* falseBlock = createNewBasicBlock(currFunc, "noShortCircuit");
    auto* resultBlock = createNewBasicBlock(currFunc, "boolExprResult");

    llvm::Value* leftVal = codegenExpression(left);
    llvm::Value* leftVali64 = builder->CreateIntCast(leftVal, llvm::Type::getInt64Ty(*context), true);
    llvm::Value* leftValBoolean = builder->CreateICmpNE(
        leftVali64, llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context), 0, true), "leftValBoolean");
    llvm::Value* branchToFalse = builder->CreateCondBr(leftValBoolean, resultBlock, falseBlock);

    auto* trueBlock = currBlock;
    switchToBlock(falseBlock);
    sealBlock(falseBlock);

    llvm::Value* rightVal = codegenExpression(right);
    auto* blockAfterRightValGeneration = currBlock;
    llvm::Value* rightVali64 = builder->CreateIntCast(rightVal, llvm::Type::getInt64Ty(*context), true);
    llvm::Value* rightValBoolean = builder->CreateICmpNE(
        rightVali64, llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context), 0), "rightValBoolean");

    llvm::Value* branchToResult = builder->CreateBr(resultBlock);
    switchToBlock(resultBlock);
    sealBlock(resultBlock);

    auto exprResult = llvm::PHINode::Create(llvm::Type::getInt1Ty(*context), 2, "boolExprResultNode");
    builder->Insert(exprResult);
    exprResult->addIncoming(llvm::ConstantInt::get(llvm::Type::getInt1Ty(*context), 1), trueBlock);
    exprResult->addIncoming(rightValBoolean, blockAfterRightValGeneration);
    return exprResult;
}

llvm::Value* SSAGenerator::codegenBinaryOp(const AST::Expression& expr) {
    auto& binExp = static_cast<const AST::BinaryOperator&>(expr);
    // special handling binops
    switch (binExp.type) {
    case TokenType::And_Logical:
        return codegenAndLogical(*binExp.operand1, *binExp.operand2);
    default:
        break; // handled later
    }

    auto* left = builder->CreateIntCast(codegenExpression(*binExp.operand1), llvm::Type::getInt64Ty(*context), true);
    auto* right = builder->CreateIntCast(codegenExpression(*binExp.operand2), llvm::Type::getInt64Ty(*context), true);
    switch (binExp.type) {
    case TokenType::And_Bit:
        return builder->CreateAnd(left, right);
    case TokenType::Plus:
        return builder->CreateAdd(left, right);
    case TokenType::Minus:
        return builder->CreateSub(left, right);
    case TokenType::Star: // if deref it would be unop not bin op
        return builder->CreateMul(left, right);
    case TokenType::Slash:
        return builder->CreateSDiv(left, right);
    case TokenType::Equals:
        return builder->CreateICmpEQ(left, right);
    case TokenType::Uneqal:
        return builder->CreateICmpNE(left, right);
    default:
        throw std::runtime_error("Unimplemented binop for codegen");
    }
}

llvm::Value* SSAGenerator::codegenUnaryOp(const AST::Expression& expr) {
    auto& unExpr = static_cast<const AST::PrefixOperator&>(expr);
    switch (unExpr.type) {
    case TokenType::Minus:
        return builder->CreateSub(llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context), 0),
                                  codegenExpression(*unExpr.operand));
    default:
        throw std::runtime_error("unimplemented unop");
    }
}

llvm::Value* SSAGenerator::codegenExpression(const AST::Expression& expr) {
    switch (expr.getType()) {
    case AST::ExpressionType::Value:
        return llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context), static_cast<const AST::Value&>(expr).val);
    case AST::ExpressionType::BinaryOperator:
        return codegenBinaryOp(expr);
    case AST::ExpressionType::Name:
        return readVariable(static_cast<const AST::Name&>(expr).literal, currBlock);
    case AST::ExpressionType::Parenthesised:
        return codegenExpression(*static_cast<const AST::Parenthesised&>(expr).inner);
    case AST::ExpressionType::PrefixOperator:
        return codegenUnaryOp(expr);
    default:
        throw std::runtime_error(std::format("Not implemented expr {}", expr.toString()));
    }
}
void SSAGenerator::codegenExprStatement(const AST::ExpressionStatement& statement) {
    codegenExpression(*statement.expression);
}

void SSAGenerator::codegenAssignment(const AST::Assignment& assignmentStatement) {
    if (assignmentStatement.left->getType() != AST::ExpressionType::Name) {
        return;
    }

    auto varName = static_cast<const AST::Name&>(*assignmentStatement.left).literal;
    auto* expr = codegenExpression(*assignmentStatement.right);

    if (assignmentStatement.modifyer) {
        if (assignmentStatement.modifyer.value().type == TokenType::Auto) {
            assert(assignmentStatement.left->getType() == AST::ExpressionType::Name);
            auto* allocaInst = builder->CreateAlloca(llvm::Type::getInt64Ty(*context));
            builder->CreateStore(expr, allocaInst);
        }
    }
    // maybe store as well?
    writeVariable(varName, currBlock, expr);
}

void SSAGenerator::codegenStatementSeq(const CFG::BasicBlock* currCFG) {
    builder->SetInsertPoint(currBlock);
    for (auto& statement : currCFG->extraInfo) {
        switch (statement->getType()) {
        case AST::ExpressionType::ExpressionStatement:
            codegenExprStatement(static_cast<const AST::ExpressionStatement&>(*statement));
            break;
        case AST::ExpressionType::Assignment:
            codegenAssignment(static_cast<const AST::Assignment&>(*statement));
            break;
        case AST::ExpressionType::Return:
            if (auto& retExpr = static_cast<const AST::Return*>(statement)->what) {
                codegenReturnSt(retExpr->get());
            } else {
                codegenReturnSt(nullptr);
            }
            break;
        default:
            throw std::runtime_error("Not implemented");
        }
    }
    codegenBlock(currCFG->posterior.at(0).get());
}

bool lastInstrInBlockIsTerminator(const llvm::BasicBlock* block) {
    return block->size() != 0 && block->rbegin()->isTerminator();
}

void SSAGenerator::codegenWhile(const CFG::BasicBlock* whileBlockCFG) {
    auto conditionBlock = createNewBasicBlock(currFunc, "conditionBlock");
    auto whileBlock = createNewBasicBlock(currFunc, "whileBlock");
    auto postWhileBlock = createNewBasicBlock(currFunc, "postWhileBlock");
    builder->CreateBr(conditionBlock);

    switchToBlock(conditionBlock);
    auto* conditionValue = codegenExpression(*whileBlockCFG->extraInfo[0]);
    auto* conditionValueAsI64 =
        builder->CreateIntCast(conditionValue, llvm::Type::getInt64Ty(*context), true, "castToI64");
    auto* conditionValueNeq0 = builder->CreateICmpNE(
        conditionValueAsI64, llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context), 0, true), "whileCondition");

    builder->CreateCondBr(conditionValueNeq0, whileBlock, postWhileBlock);

    switchToBlock(whileBlock);
    sealBlock(whileBlock);
    codegenBlock(whileBlockCFG->posterior[0].get());

    if (!lastInstrInBlockIsTerminator(currBlock))
        builder->CreateBr(conditionBlock);

    sealBlock(conditionBlock);

    switchToBlock(postWhileBlock);
    sealBlock(postWhileBlock);
    codegenBlock(whileBlockCFG->posterior[1].get());
}

void SSAGenerator::codegenIf(const CFG::BasicBlock* ifBlock) {
    auto* conditionValue = codegenExpression(*ifBlock->extraInfo[0]);
    auto* conditionValueAsI64 =
        builder->CreateIntCast(conditionValue, llvm::Type::getInt64Ty(*context), true, "castToI64");
    auto* conditionValueNeq0 = builder->CreateICmpNE(
        conditionValueAsI64, llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context), 0, true), "ifCondition");
    auto* thenBranchBlock = createNewBasicBlock(currFunc, "thenBranch");
    llvm::BasicBlock* elseBranchBlock = nullptr;
    if (ifBlock->posterior[1]) {
        elseBranchBlock = createNewBasicBlock(currFunc, "elseBranch");
        builder->CreateCondBr(conditionValueNeq0, thenBranchBlock, elseBranchBlock);
    }

    auto* postIfBlock = createNewBasicBlock(currFunc, "postIf");
    if (!elseBranchBlock) {
        builder->CreateCondBr(conditionValueNeq0, thenBranchBlock, postIfBlock);
    }

    switchToBlock(thenBranchBlock);
    sealBlock(thenBranchBlock);
    codegenBlock(ifBlock->posterior[0].get());

    if (!lastInstrInBlockIsTerminator(currBlock))
        builder->CreateBr(postIfBlock);

    if (elseBranchBlock) {
        switchToBlock(elseBranchBlock);
        sealBlock(elseBranchBlock);
        codegenBlock(ifBlock->posterior[1].get());
        if (!lastInstrInBlockIsTerminator(currBlock))
            builder->CreateBr(postIfBlock);
        sealBlock(elseBranchBlock);
    }

    if (llvm::pred_size(postIfBlock) == 0) {
        llvm::DeleteDeadBlock(postIfBlock);
    } else {
        switchToBlock(postIfBlock);
        sealBlock(postIfBlock);
        codegenBlock(ifBlock->posterior[2].get());
    }
}

void SSAGenerator::codegenReturnSt(const AST::Expression* retVal) {
    if (retVal) {
        builder->CreateRet(codegenExpression(*retVal));
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
        codegenStatementSeq(currCFG);
        break;
    case CFG::BlockType::FunctionEpilogue:
        break;
    case CFG::BlockType::Return:
        if (!currBlock->rbegin()->isTerminator()) // this is conservatively added at too many places, if we already have
                                                  // a terminator then just ignore
            codegenReturnSt(currCFG->extraInfo.at(0));
        break;
    case CFG::BlockType::If:
        codegenIf(currCFG);
        break;
    case CFG::BlockType::While:
        codegenWhile(currCFG);
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

    auto* entryBlock = createNewBasicBlock(currFunc, "entry");
    currBlock = entryBlock;

    auto argIt = currFunc->arg_begin();
    for (size_t i = 0; i < numParams; ++i) {
        writeVariable(paramNames.at(i), currBlock, argIt);
        (argIt++)->setName(paramNames.at(i));
    }

    builder->SetInsertPoint(currBlock);

    sealBlock(entryBlock);
    codegenBlock(prelude);

    if (!CFG::doesFunctionHaveNonVoidReturnType(prelude)) {
        // add empty ret if it is not there already
        if (currBlock->size() != 0) {
            if (!currBlock->rbegin()->isTerminator()) {
                builder->CreateRetVoid();
            }
        } else {
            builder->CreateRetVoid();
        }
    }

    currFunc->dump();
    if (llvm::verifyFunction(*currFunc, &llvm::errs())) {
        throw std::runtime_error("Invalid function");
    }
}