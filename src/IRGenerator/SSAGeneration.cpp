#include "SSAGeneration.h"
#include "ValueTracker.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Verifier.h"

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

        auto* expLeft = generateExpression(*binOp.operand1);
        auto* expRight = generateExpression(*binOp.operand2);

        // ints and same size necessary
        expLeft = castToInt64(expLeft);
        expRight = castToInt64(expRight);

        switch (binOp.type) {
        case TokenType::Plus:
            return builder.CreateAdd(expLeft, expRight);
        case TokenType::Minus:
            return builder.CreateSub(expLeft, expRight);
        case TokenType::Uneqal:
            return builder.CreateICmpNE(expLeft, expRight);
        }
        throw std::runtime_error("Unimplemented IR gen of binop");
    }

    llvm::Constant* fromInt(std::int64_t val, llvm::Type* type = nullptr) {
        if (!type)
            return llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context), val, true);
        auto* intType = llvm::cast<llvm::IntegerType>(type);
        return llvm::ConstantInt::get(intType, val, true);
    }

    llvm::Value* generateUnaryOperation(const AST::PrefixOperator& unOp) {
        switch (unOp.type) {
        case TokenType::And_Bit: {
            assert(unOp.operand->getType() == AST::ExpressionType::Name ||
                   nOp.operand->getType() == AST::ExpressionType::ArrayIndexing);
            if (unOp.operand->getType() == AST::ExpressionType::Name) {
                llvm::Value* val =
                    valueTracker.readVariable(static_cast<const AST::Name&>(*unOp.operand).literal, currBlock);
                if (val->getType()->isIntegerTy()) {
                    val = builder.CreateIntToPtr(val, llvm::PointerType::get(*context, 0));
                }
                return builder.CreateGEP(val->getType(), val, {});
            } else {
                
            }
        }
        default:
            break;
        }
        // normals
        llvm::Value* inner = generateExpression(*unOp.operand);
        switch (unOp.type) {
        case TokenType::Minus:
            return builder.CreateNeg(inner);
        case TokenType::Tilde:
            return builder.CreateNot(inner);
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
        builder.SetInsertPoint(&currFunc->getEntryBlock());
        auto* alloca = builder.CreateAlloca(type);
        builder.SetInsertPoint(currBlock);
        return alloca;
    }

    llvm::Value* generateAssignment(const AST::AssignmentExpr& assExpr) {
        llvm::Value* rightSide = generateExpression(*assExpr.right);
        if (assExpr.left->getType() != AST::ExpressionType::Name) {
            const auto& arrayIndexing = static_cast<const AST::ArrayIndexing&>(*assExpr.left);

            auto* arrayPointer = generateExpression(*arrayIndexing.array);
            if (arrayPointer->getType()->isIntegerTy()) {
                arrayPointer = builder.CreateIntToPtr(arrayPointer, llvm::PointerType::get(*context, 0));
            }
            bool isSizespec = [&]() {
                if (arrayIndexing.index->getType() == AST::ExpressionType::BinaryOperator) {
                    const auto& binop = static_cast<const AST::BinaryOperator&>(*arrayIndexing.index);
                    if (binop.type == TokenType::Sizespec) {
                        return true;
                    }
                }
                return false;
            }();

            llvm::Value* pointerToAssignTo = nullptr;
            if (!isSizespec) {
                llvm::Value* index = generateExpression(*arrayIndexing.index);
                pointerToAssignTo = builder.CreateGEP(llvm::Type::getInt64Ty(*context), arrayPointer, {index});
                builder.CreateStore(rightSide, pointerToAssignTo);
            } else {
                const auto& binop = static_cast<const AST::BinaryOperator&>(*arrayIndexing.index);
                assert(binop.operand2->getType() == AST::ExpressionType::Value);
                std::uint8_t sizespec = static_cast<const AST::Value&>(*binop.operand2).val;
                llvm::Value* scalee = generateExpression(*binop.operand1);
                llvm::Type* type = [sizespec, &context = *context]() {
                    switch (sizespec) {
                    case 1:
                        return llvm::Type::getInt8Ty(context);
                    case 2:
                        return llvm::Type::getInt16Ty(context);
                    case 3:
                        return llvm::Type::getInt32Ty(context);
                    case 4:
                        return llvm::Type::getInt64Ty(context);
                    default:
                        throw std::runtime_error("erroneous sizespec");
                    }
                }();
                pointerToAssignTo = builder.CreateGEP(type, arrayPointer, {scalee});
                auto* truncatedRightSide = builder.CreateTrunc(rightSide, type);
                builder.CreateStore(truncatedRightSide, pointerToAssignTo);
            }

        } else {
            std::string_view assignedVar = static_cast<const AST::Name&>(*assExpr.left).literal;
            // this variable has to have been declared already
            llvm::Value* currVal = valueTracker.readVariable(assignedVar, currBlock);
            if (llvm::AllocaInst* alloca = dyn_cast<llvm::AllocaInst>(currVal)) {
                builder.CreateStore(rightSide, alloca);
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

    llvm::Value* generateExpression(const AST::Expression& expr) {
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
            llvm::Value* val = valueTracker.readVariable(static_cast<const AST::Name&>(expr).literal, currBlock);
            if (auto* alloca = llvm::dyn_cast<llvm::AllocaInst>(val)) {
                return builder.CreateLoad(llvm::Type::getInt64Ty(*context), alloca);
            }
            return val;
        }
        case AST::ExpressionType::AssignmentExpr:
            return generateAssignment(static_cast<const AST::AssignmentExpr&>(expr));
        }
        throw std::runtime_error(std::format("IR gen for expression {} unimplemented", expr.toString()));
    }

    llvm::Value* createSextIfNecessary(llvm::Value* castee, llvm::Type* type) {
        if (castee->getType() == type)
            return castee;
        return builder.CreateSExt(castee, type);
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
            return generateDeclaration(static_cast<const AST::Assignment&>(statement));
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
        // generateStatement(func.body[0]);
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
