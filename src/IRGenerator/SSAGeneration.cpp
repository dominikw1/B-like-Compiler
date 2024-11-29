#include "SSAGeneration.h"
#include "ValueTracker.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

namespace {

std::vector<std::string_view> extractParameterNamesFromFunction(const AST::Function& function) {
    const auto& paramNode = function.argList;
    if (!paramNode) {
        return {};
    }
    assert(paramNode.value()->getType() == AST::ExpressionType::Parenthesised);
    AST::Parenthesised& paren = static_cast<AST::Parenthesised&>(*paramNode.value());

    assert(paren.inner && paren.inner.value());
    assert(paren.inner.value()->getType() == AST::ExpressionType::Name ||
           paren.inner.value()->getType() == AST::ExpressionType::CommaList);

    if (paren.inner.value()->getType() == AST::ExpressionType::Name) {
        return {static_cast<AST::Name&>(*paren.inner.value()).literal};
    } else {
        std::vector<std::string_view> names;
        AST::Node* curr = &paren.inner.value();
        while (true) {
            assert((*curr)->getType() == AST::ExpressionType::Name ||
                   (*curr)->getType() == AST::ExpressionType::CommaList);
            if ((*curr)->getType() == AST::ExpressionType::Name) {
                names.push_back(static_cast<const AST::Name&>(**curr).literal);
                break;
            } else {
                AST::CommaList& list = static_cast<AST::CommaList&>(**curr);
                assert(list.left->getType() == AST::ExpressionType::Name);
                names.push_back(static_cast<const AST::Name&>(*list.left).literal);
                curr = &list.right;
            }
        }
        return names;
    }
}

class SSAGenerator {
  private:
    std::unique_ptr<llvm::LLVMContext> context = std::make_unique<llvm::LLVMContext>();
    std::unique_ptr<llvm::Module> module = std::make_unique<llvm::Module>("main_module", *context);
    llvm::IRBuilder<> builder{*context};
    llvm::Function* currFunc = nullptr;
    llvm::BasicBlock* currBlock = nullptr;
    std::unordered_set<std::string_view> undecidedFunctionReturnTypes{};
    ValueTracker valueTracker{*context};

    struct SizespecInfo {
        llvm::Value* pointerRes;
        llvm::Type* pointerType;
    };

    constexpr bool isSizespec(const AST::ArrayIndexing& arrayIndexing) {
        if (arrayIndexing.index->getType() == AST::ExpressionType::BinaryOperator) {
            const auto& binop = static_cast<const AST::BinaryOperator&>(*arrayIndexing.index);
            if (binop.type == TokenType::Sizespec) {
                return true;
            }
        }
        return false;
    }

    SizespecInfo handleSizespec(llvm::Value* arrayPointer, const AST::BinaryOperator& sizespec) {
        assert(sizespec.operand2->getType() == AST::ExpressionType::Value);
        std::uint8_t sizespecSize = static_cast<const AST::Value&>(*sizespec.operand2).val;
        llvm::Value* scalee = generateExpression(*sizespec.operand1);
        llvm::Type* type = [sizespecSize, &context = *context]() {
            switch (sizespecSize) {
            case 1:
                return llvm::Type::getInt8Ty(context);
            case 2:
                return llvm::Type::getInt16Ty(context);
            case 4:
                return llvm::Type::getInt32Ty(context);
            case 8:
                return llvm::Type::getInt64Ty(context);
            default:
                throw std::runtime_error("erroneous sizespec");
            }
        }();
        return SizespecInfo{builder.CreateGEP(type, arrayPointer, {scalee}), type};
    }

    llvm::Value* castToInt64(llvm::Value* v) {
        if (v->getType() == llvm::Type::getInt64Ty(*context)) {
            return v;
        }
        if (v->getType()->isPointerTy()) {
            return builder.CreatePtrToInt(v, llvm::Type::getInt64Ty(*context));
        }
        if (v->getType() == llvm::Type::getInt1Ty(*context)) {
            // if we stupidly sext this then we get bogus :c
            return builder.CreateZExt(v, llvm::Type::getInt64Ty(*context));
        }
        return builder.CreateSExt(v, llvm::Type::getInt64Ty(*context));
    }

    llvm::Value* generateBinaryOperation(const AST::BinaryOperator& binOp) {

        switch (binOp.type) {
        case TokenType::And_Logical:
            return generateAndLogical(*binOp.operand1, *binOp.operand2);
        case TokenType::Or_Logical:
            return generateOrLogical(*binOp.operand1, *binOp.operand2);
        default:
            break;
        }

        auto* expLeft = generateExpression(*binOp.operand1);
        auto* expRight = generateExpression(*binOp.operand2);
        // ints and same size necessary
        expLeft = castToInt64(expLeft);
        expRight = castToInt64(expRight);

        switch (binOp.type) {
        case TokenType::Larger:
            return builder.CreateICmpSGT(expLeft, expRight);
        case TokenType::Smaller:
            return builder.CreateICmpSLT(expLeft, expRight);
        case TokenType::Larger_Equal:
            return builder.CreateICmpSGE(expLeft, expRight);
        case TokenType::Smaller_Equal:
            return builder.CreateICmpSGE(expLeft, expRight);
        case TokenType::Plus:
            return builder.CreateAdd(expLeft, expRight);
        case TokenType::Minus:
            return builder.CreateSub(expLeft, expRight);
        case TokenType::Uneqal:
            return builder.CreateICmpNE(expLeft, expRight);
        case TokenType::Equals:
            return builder.CreateICmpEQ(expLeft, expRight);
        case TokenType::Star:
            return builder.CreateMul(expLeft, expRight);
        case TokenType::Xor:
            return builder.CreateXor(expLeft, expRight);
        case TokenType::Slash:
            return builder.CreateSDiv(expLeft, expRight);
        case TokenType::And_Bit:
            return builder.CreateAnd(expLeft, expRight);
        case TokenType::Or_Bit:
            return builder.CreateOr(expLeft, expRight);
        case TokenType::Bitshift_Left:
            return builder.CreateShl(expLeft, expRight);
        case TokenType::Bitshift_Right:
            return builder.CreateAShr(expLeft, expRight);
        case TokenType::Mod:
            return builder.CreateSRem(expLeft, expRight);
        }
        throw std::runtime_error("Unimplemented IR gen of binop");
    }

    llvm::Constant* fromInt(std::int64_t val, llvm::Type* type = nullptr) {
        if (!type)
            return llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context), val, true);
        auto* intType = llvm::cast<llvm::IntegerType>(type);
        return llvm::ConstantInt::get(intType, val, true);
    }

    llvm::Value* generateAddressOf(const AST::PrefixOperator& unOp) {
        assert(unOp.operand->getType() == AST::ExpressionType::Name ||
               unOp.operand->getType() == AST::ExpressionType::ArrayIndexing);
        if (unOp.operand->getType() == AST::ExpressionType::Name) {
            llvm::Value* val =
                valueTracker.readVariable(static_cast<const AST::Name&>(*unOp.operand).literal,
                                          static_cast<const AST::Name&>(*unOp.operand).isAlloca, currBlock);
            if (val->getType()->isIntegerTy()) {
                val = builder.CreateIntToPtr(val, llvm::PointerType::get(*context, 0));
            }
            return builder.CreateGEP(val->getType(), val, {});
        } else {
            const auto& arrayIndexing = static_cast<const AST::ArrayIndexing&>(*unOp.operand);
            auto* arrayPointer = generateExpression(*arrayIndexing.array);
            if (arrayPointer->getType()->isIntegerTy()) {
                arrayPointer = builder.CreateIntToPtr(arrayPointer, llvm::PointerType::get(*context, 0));
            }
            if (!isSizespec(arrayIndexing)) {
                llvm::Value* index = generateExpression(*arrayIndexing.index);
                return builder.CreateGEP(llvm::Type::getInt64Ty(*context), arrayPointer, {index});
            } else {
                auto ssInfo =
                    handleSizespec(arrayPointer, static_cast<const AST::BinaryOperator&>(*arrayIndexing.index));
                return ssInfo.pointerRes;
            }
        }
    }

    llvm::Value* generateUnaryOperation(const AST::PrefixOperator& unOp) {
        switch (unOp.type) {
        case TokenType::And_Bit:
            return generateAddressOf(unOp);
        default:
            break;
        }
        // normals
        llvm::Value* inner = generateExpression(*unOp.operand);
        switch (unOp.type) {
        case TokenType::Minus:
            return builder.CreateNeg(castToInt64(inner));
        case TokenType::Tilde:
            return builder.CreateNot(castToInt64(inner));
        case TokenType::Exclamation_Mark:
            return builder.CreateICmpEQ(inner, fromInt(0, inner->getType()));
        default:
            break;
        }
        throw std::runtime_error(std::format("Unimplemented IR gen of unop {}", unOp.sExpression()));
    }

    llvm::Value* codegenFunctionCall(const AST::FunctionCall& expr) {
        const auto& name = static_cast<const AST::Name&>(*expr.name).literal;
        std::vector<llvm::Value*> args{};
        if (expr.args) {
            assert(expr.args.value());
            const auto* argNode = static_cast<const AST::Parenthesised&>(*expr.args.value()).inner.value().get();
            while (argNode) {
                if (argNode->getType() == AST::ExpressionType::CommaList) {
                    const auto& list = static_cast<const AST::CommaList&>(*argNode);
                    auto* expr = generateExpression(*list.left);
                    if (expr->getType()->isPointerTy()) {
                        expr = builder.CreatePtrToInt(expr, llvm::Type::getInt64Ty(*context));
                    }
                    args.push_back(expr);
                    argNode = list.right.get();
                } else {
                    auto* expr = generateExpression(*argNode);
                    if (expr->getType()->isPointerTy()) {
                        expr = builder.CreatePtrToInt(expr, llvm::Type::getInt64Ty(*context));
                    }
                    args.push_back(expr);
                    break;
                }
            }
        }

        auto* calledFunc = module->getFunction(name);
        if (!calledFunc) {
            // implicit function declaration
            size_t numParams = args.size();
            std::vector<llvm::Type*> parameters(numParams, llvm::Type::getInt64Ty(*context));
            undecidedFunctionReturnTypes.insert(name);
            auto type = llvm::FunctionType::get(llvm::Type::getInt64Ty(*context), parameters, false);
            calledFunc = llvm::cast<llvm::Function>(module->getOrInsertFunction(name, type).getCallee());
        }
        return builder.CreateCall(calledFunc, args);
    }

    llvm::Value* createAlloca(llvm::Type* type) {
        if (currFunc->getEntryBlock().empty())
            builder.SetInsertPoint(&currFunc->getEntryBlock());
        else
            builder.SetInsertPoint(&*currFunc->getEntryBlock().begin());
        auto* alloca = builder.CreateAlloca(type);
        builder.SetInsertPoint(currBlock);
        return alloca;
    }

    llvm::Value* generateAndLogical(const AST::Expression& left, const AST::Expression& right) {
        auto* trueBlock = llvm::BasicBlock::Create(*context, "noShortCircuit", currFunc);
        auto* resultBlock = llvm::BasicBlock::Create(*context, "boolExprResult", currFunc);

        llvm::Value* leftVal = castToInt64(generateExpression(left));
        llvm::Value* leftValBoolean = builder.CreateICmpEQ(
            leftVal, llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context), 0, true), "leftValBoolean");
        llvm::Value* branchToFalse = builder.CreateCondBr(leftValBoolean, resultBlock, trueBlock);

        auto* falseBlock = currBlock;
        switchBlock(trueBlock);
        valueTracker.sealBlock(trueBlock);

        llvm::Value* rightVal = castToInt64(generateExpression(right));
        auto* blockAfterRightValGeneration = currBlock;
        llvm::Value* rightValBoolean = builder.CreateICmpNE(
            rightVal, llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context), 0), "rightValBoolean");

        llvm::Value* branchToResult = builder.CreateBr(resultBlock);
        switchBlock(resultBlock);
        valueTracker.sealBlock(resultBlock);

        auto exprResult = llvm::PHINode::Create(llvm::Type::getInt1Ty(*context), 2, "boolExprResultNode");
        builder.Insert(exprResult);
        exprResult->addIncoming(llvm::ConstantInt::get(llvm::Type::getInt1Ty(*context), 0), falseBlock);
        exprResult->addIncoming(rightValBoolean, blockAfterRightValGeneration);
        return exprResult;
    }

    llvm::Value* generateOrLogical(const AST::Expression& left, const AST::Expression& right) {
        auto* falseBlock = llvm::BasicBlock::Create(*context, "noShortCircuit", currFunc);
        auto* resultBlock = llvm::BasicBlock::Create(*context, "boolExprResult", currFunc);

        llvm::Value* leftVal = castToInt64(generateExpression(left));
        llvm::Value* leftValBoolean = builder.CreateICmpNE(
            leftVal, llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context), 0, true), "leftValBoolean");
        llvm::Value* branchToFalse = builder.CreateCondBr(leftValBoolean, resultBlock, falseBlock);

        auto* trueBlock = currBlock;
        switchBlock(falseBlock);
        valueTracker.sealBlock(falseBlock);

        llvm::Value* rightVal = castToInt64(generateExpression(right));
        auto* blockAfterRightValGeneration = currBlock;
        llvm::Value* rightValBoolean = builder.CreateICmpNE(
            rightVal, llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context), 0), "rightValBoolean");

        llvm::Value* branchToResult = builder.CreateBr(resultBlock);
        switchBlock(resultBlock);
        valueTracker.sealBlock(resultBlock);

        auto exprResult = llvm::PHINode::Create(llvm::Type::getInt1Ty(*context), 2, "boolExprResultNode");
        builder.Insert(exprResult);
        exprResult->addIncoming(llvm::ConstantInt::get(llvm::Type::getInt1Ty(*context), 1), trueBlock);
        exprResult->addIncoming(rightValBoolean, blockAfterRightValGeneration);
        return exprResult;
    }

    llvm::Value* generateAssignment(const AST::AssignmentExpr& assExpr) {
        llvm::Value* rightSide = generateExpression(*assExpr.right);
        if (assExpr.left->getType() != AST::ExpressionType::Name) {
            const auto& arrayIndexing = static_cast<const AST::ArrayIndexing&>(*assExpr.left);

            auto* arrayPointer = generateExpression(*arrayIndexing.array);
            if (arrayPointer->getType()->isIntegerTy()) {
                arrayPointer = builder.CreateIntToPtr(arrayPointer, llvm::PointerType::get(*context, 0));
            }

            llvm::Value* pointerToAssignTo = nullptr;
            if (!isSizespec(arrayIndexing)) {
                llvm::Value* index = generateExpression(*arrayIndexing.index);
                pointerToAssignTo = builder.CreateGEP(llvm::Type::getInt64Ty(*context), arrayPointer, {index});
                builder.CreateStore(rightSide, pointerToAssignTo);
            } else {
                const auto& binop = static_cast<const AST::BinaryOperator&>(*arrayIndexing.index);
                auto ssInfo = handleSizespec(arrayPointer, binop);
                pointerToAssignTo = ssInfo.pointerRes;
                auto* truncatedRightSide = builder.CreateTrunc(rightSide, ssInfo.pointerType);
                builder.CreateStore(truncatedRightSide, pointerToAssignTo);
            }
        } else {
            std::string_view assignedVar = static_cast<const AST::Name&>(*assExpr.left).literal;
            // this variable has to have been declared already
            llvm::Value* currVal = valueTracker.readVariable(
                assignedVar, static_cast<const AST::Name&>(*assExpr.left).isAlloca, currBlock);
            if (static_cast<const AST::Name&>(*assExpr.left).isAlloca) {
                builder.CreateStore(rightSide, currVal);
            } else {
                valueTracker.writeVariable(assignedVar, currBlock, rightSide);
            }
        }
        return rightSide;
    }

    llvm::Value* generateDeclaration(const AST::Assignment& decl) {
        assert(decl.modifyer);
        assert(decl.left->getType() == AST::ExpressionType::Name);
        std::string_view name = static_cast<const AST::Name&>(*decl.left).literal;
        auto* rightSide = generateExpression(*decl.right);
        if (decl.modifyer.value().type == TokenType::Auto) {
            auto* alloca = createAlloca(llvm::Type::getInt64Ty(*context));
            builder.CreateStore(rightSide, alloca);
            valueTracker.writeVariable(name, currBlock, alloca);
        } else {
            valueTracker.writeVariable(name, currBlock, rightSide);
        }
        return rightSide;
    }

    void generateWhile(const AST::While& whileStatement) {
        auto conditionBlock = llvm::BasicBlock::Create(*context, "conditionBlock", currFunc);
        auto whileBlock = llvm::BasicBlock::Create(*context, "whileBlock", currFunc);
        auto postWhileBlock = llvm::BasicBlock::Create(*context, "postWhileBlock", currFunc);
        builder.CreateBr(conditionBlock);

        switchBlock(conditionBlock);
        auto* conditionValue = castToInt64(generateExpression(*whileStatement.condition));
        auto* conditionValueNeq0 = builder.CreateICmpNE(
            conditionValue, llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context), 0, true), "whileCondition");

        builder.CreateCondBr(conditionValueNeq0, whileBlock, postWhileBlock);

        switchBlock(whileBlock);
        valueTracker.sealBlock(whileBlock);
        generateExpression(*whileStatement.body);

        if (!currBlock->getTerminator())
            builder.CreateBr(conditionBlock);

        valueTracker.sealBlock(conditionBlock);

        switchBlock(postWhileBlock);
        valueTracker.sealBlock(postWhileBlock);
    }

    llvm::Value* generateExpression(const AST::Expression& expr) {
        if (expr.isStatement()) {
            generateStatement(static_cast<const AST::Statement&>(expr));
            // in this branch the value shall never be used! This is only called if we are coming from an if / while
            // which discards the return value
            return nullptr;
        }
        switch (expr.getType()) {
        case AST::ExpressionType::Value:
            return llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context), static_cast<const AST::Value&>(expr).val,
                                          true);
        case AST::ExpressionType::BinaryOperator:
            return generateBinaryOperation(static_cast<const AST::BinaryOperator&>(expr));

        case AST::ExpressionType::PrefixOperator:
            return generateUnaryOperation(static_cast<const AST::PrefixOperator&>(expr));
        case AST::ExpressionType::Parenthesised: {
            const AST::Parenthesised& par = static_cast<const AST::Parenthesised&>(expr);
            if (par.inner) {
                return generateExpression(*par.inner.value());
            }
            throw std::runtime_error("empty parentheses expr");
        }
        case AST::ExpressionType::FunctionCall:
            return codegenFunctionCall(static_cast<const AST::FunctionCall&>(expr));
        case AST::ExpressionType::Name: {
            llvm::Value* val = valueTracker.readVariable(static_cast<const AST::Name&>(expr).literal,
                                                         static_cast<const AST::Name&>(expr).isAlloca, currBlock);
            if (static_cast<const AST::Name&>(expr).isAlloca) {
                return builder.CreateLoad(llvm::Type::getInt64Ty(*context), val);
            }
            return val;
        }
        case AST::ExpressionType::AssignmentExpr:
            return generateAssignment(static_cast<const AST::AssignmentExpr&>(expr));
        case AST::ExpressionType::ArrayIndexing: {
            const auto& arrayIndexing = static_cast<const AST::ArrayIndexing&>(expr);
            auto* arrayPointer = generateExpression(*arrayIndexing.array);
            if (arrayPointer->getType()->isIntegerTy()) {
                arrayPointer = builder.CreateIntToPtr(arrayPointer, llvm::PointerType::get(*context, 0));
            }

            if (!isSizespec(arrayIndexing)) {
                llvm::Value* index = generateExpression(*arrayIndexing.index);
                auto* ptr = builder.CreateGEP(llvm::Type::getInt64Ty(*context), arrayPointer, {index});
                return builder.CreateLoad(llvm::Type::getInt64Ty(*context), ptr);
            } else {
                const auto& binop = static_cast<const AST::BinaryOperator&>(*arrayIndexing.index);
                auto ssInfo = handleSizespec(arrayPointer, binop);
                auto* loaded = builder.CreateLoad(ssInfo.pointerType, ssInfo.pointerRes);
                return castToInt64(loaded);
            }
        }
        }

        throw std::runtime_error(std::format("IR gen for expression {} unimplemented", expr.toString()));
    }

    llvm::Value* createSextIfNecessary(llvm::Value* castee, llvm::Type* type) {
        if (castee->getType() == type)
            return castee;
        return builder.CreateSExt(castee, type);
    }

    void switchBlock(llvm::BasicBlock* newBlock) {
        currBlock = newBlock;
        builder.SetInsertPoint(newBlock);
    }

    bool generateIfStatement(const AST::If& ifStatement) {
        auto* conditionValue = castToInt64(generateExpression(*ifStatement.condition));
        auto* conditionValueNeq0 = builder.CreateICmpNE(
            conditionValue, llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context), 0, true), "ifCondition");

        auto* thenBranchBlock = llvm::BasicBlock::Create(*context, "thenBranch", currFunc);
        llvm::BasicBlock* elseBranchBlock = nullptr;
        if (ifStatement.elseBranch) {
            elseBranchBlock = llvm::BasicBlock::Create(*context, "elseBranch", currFunc);
            builder.CreateCondBr(conditionValueNeq0, thenBranchBlock, elseBranchBlock);
        }
        auto* postIfBlock = llvm::BasicBlock::Create(*context, "postIf", currFunc);
        if (!elseBranchBlock) {
            builder.CreateCondBr(conditionValueNeq0, thenBranchBlock, postIfBlock);
        }

        switchBlock(thenBranchBlock);
        valueTracker.sealBlock(thenBranchBlock);
        generateExpression(*ifStatement.thenBranch);

        bool thenHadTerminator = false;
        bool elseHadTerminator = false;
        if (!currBlock->getTerminator())
            builder.CreateBr(postIfBlock);
        else
            thenHadTerminator = true;

        if (elseBranchBlock) {
            switchBlock(elseBranchBlock);
            valueTracker.sealBlock(elseBranchBlock);
            assert(ifStatement.elseBranch);
            generateExpression(*ifStatement.elseBranch.value());
            if (!currBlock->getTerminator())
                builder.CreateBr(postIfBlock);
            else
                elseHadTerminator = true;
            valueTracker.sealBlock(elseBranchBlock);
        }

        if (llvm::pred_size(postIfBlock) == 0 || (thenHadTerminator && elseHadTerminator)) {
            llvm::DeleteDeadBlock(postIfBlock);
            return false;
        } else {
            switchBlock(postIfBlock);
            valueTracker.sealBlock(postIfBlock);
        }
        return true;
    }

    bool generateStatement(const AST::Statement& statement) {
        switch (statement.getType()) {
        case AST::ExpressionType::Return: {
            const AST::Return& returnStatement = static_cast<const AST::Return&>(statement);
            if (returnStatement.what) {
                builder.CreateRet(createSextIfNecessary(generateExpression(*returnStatement.what.value()),
                                                        llvm::Type::getInt64Ty(*context)));
            } else {
                builder.CreateRetVoid();
            }
            return false;
        }
        case AST::ExpressionType::ExpressionStatement: {
            const AST::ExpressionStatement& exprSt = static_cast<const AST::ExpressionStatement&>(statement);
            generateExpression(*exprSt.expression);
            return true;
        }
        case AST::ExpressionType::Assignment:
            generateDeclaration(static_cast<const AST::Assignment&>(statement));
            return true;
        case AST::ExpressionType::If:
            return generateIfStatement(static_cast<const AST::If&>(statement));
        case AST::ExpressionType::While:
            generateWhile(static_cast<const AST::While&>(statement));
            return true;
        case AST::ExpressionType::Scope: {
            for (auto& statement : static_cast<const AST::Scope&>(statement).scoped) {
                if (!generateStatement(static_cast<const AST::Statement&>(*statement))) {
                    break; // return false?
                }
            }
            return true;
        }
        default:
            throw std::runtime_error(
                std::format("Unknown statement type while generating IR for statement {}", statement.sExpression()));
        }
    }

    void generateFunction(const AST::Function& func) {
        std::string_view funcName = static_cast<const AST::Name&>(*func.name).literal;
        auto paramNames = extractParameterNamesFromFunction(func);
        size_t numParams = paramNames.size();
        if (undecidedFunctionReturnTypes.contains(funcName)) {
            if (func.isVoid) {
                std::vector<llvm::Type*> parameters(numParams, llvm::Type::getInt64Ty(*context));
                auto correctType = llvm::FunctionType::get(llvm::Type::getVoidTy(*context), parameters, false);
                module->getFunction(funcName)->mutateType(correctType);
            }
        }
        if (auto* func = module->getFunction(funcName)) {
            std::cerr << "function already exists...\n";
        }
        std::vector<llvm::Type*> parameters(numParams, llvm::Type::getInt64Ty(*context));
        auto type = llvm::FunctionType::get(
            !func.isVoid ? llvm::Type::getInt64Ty(*context) : llvm::Type::getVoidTy(*context), parameters, false);
        llvm::FunctionCallee funcCallee = module->getOrInsertFunction(funcName, type);

        currFunc = dyn_cast<llvm::Function>(funcCallee.getCallee());
        currBlock = llvm::BasicBlock::Create(*context, "entry", currFunc);
        valueTracker.sealBlock(currBlock);

        auto argIt = currFunc->arg_begin();
        for (size_t i = 0; i < numParams; ++i) {
            valueTracker.writeVariable(paramNames.at(i), currBlock, argIt);
            (argIt++)->setName(paramNames.at(i));
        }

        builder.SetInsertPoint(currBlock);
        for (auto& statement : func.body) {
            assert(statement->isStatement());
            if (!generateStatement(static_cast<AST::Statement&>(*statement))) {
                // terminator in middle of function -> no need to gen further
                break;
            }
        }

        if (!currBlock->getTerminator()) {
            builder.CreateRetVoid();
        }
    }

  public:
    SSAGenerator(AST::AST ast) {
        for (const auto& func : ast.getTopLevel()) {
            assert(func->getType() == AST::ExpressionType::Function);
            generateFunction(static_cast<const AST::Function&>(*func));
        }
    }
    IntermediateRepresentation releaseIR() { return IntermediateRepresentation{std::move(context), std::move(module)}; }
};

} // namespace

IntermediateRepresentation generateIR(AST::AST ast) {
    SSAGenerator ssaGen{std::move(ast)};
    auto IR = ssaGen.releaseIR();
    if (llvm::verifyModule(*IR.module, &llvm::errs())) {
        IR.module->print(llvm::errs(), nullptr);

        throw std::runtime_error("Invalid IR!");
    }
    return IR;
}
