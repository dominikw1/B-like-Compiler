#include "AST.h"
#include <string>
#include <unordered_set>

#define ASSERT_NAME_IS_NOT_FUNCTION_IF_NAME(node, scope)                                                               \
                                                                                                                       \
    do {                                                                                                               \
        if (NODE_IS(node, Name)) {                                                                                     \
            auto& lit = NODE_AS_REF(node, Name).literal;                                                               \
            if (scope.functions.contains(std::string(lit))) {                                                          \
                throw std::runtime_error("Function found where variable expected");                                    \
            }                                                                                                          \
        }                                                                                                              \
    } while (0);
namespace AST {
void AST::analyze() const {
    SymbolScope scope{};
    for (auto& func : toplevel) {
        auto& funAsFunc = NODE_AS_REF(func, Function);
        if (scope.functions.contains(std::string(NODE_AS_REF(funAsFunc.name, Name).literal))) {
            throw std::runtime_error("Redefinition of function");
        }
        std::uint32_t argCnt = [&]() -> std::uint32_t {
            if (!funAsFunc.argList) {
                return 0;
            }
            if (!NODE_IS(NODE_AS_REF(funAsFunc.argList.value(), Parenthesised).inner, CommaList)) {
                return 1;
            }
            return NODE_AS_REF(NODE_AS_REF(funAsFunc.argList.value(), Parenthesised).inner, CommaList).getNumInList();
        }();
        scope.functions[std::string(NODE_AS_REF(funAsFunc.name, Name).literal)] = FunctionSymbol{.numArgs = argCnt};
        func->doAnalysis(scope, 0);
    }
}

void Value::doAnalysis(SymbolScope scope, std::uint32_t depth) const {}

void Name::doAnalysis(SymbolScope scope, std::uint32_t depth) const {
    if (scope.functions.count(std::string(literal)) == 0 && scope.variables.contains(std::string(literal)) == 0) {
        scope.dump();
        throw std::runtime_error(std::format("Referenced {} not in scope", literal));
    }
}

PrefixOperator::PrefixOperator(TokenType type, Node operand) : type{type}, operand{std::move(operand)} {
    if (!this->operand) {
        throw std::runtime_error(std::format("Malformed prefix operation of type {}", tokenTypeToString(type)));
    }
    if (type == TokenType::And_Bit) {
        if (!NODE_IS(this->operand, Name) && !NODE_IS(this->operand, ArrayIndexing)) {
            throw std::runtime_error("Operand of address-of operator must be of form identifier or identifier[expr]");
        }
        if (NODE_IS(this->operand, ArrayIndexing)) {
            auto& indexing = NODE_AS_REF(this->operand, ArrayIndexing);
            auto* indexExpr = CAST_NODE_IF_TYPE(indexing.index, BinaryOperator);
            if (indexExpr && indexExpr->type == TokenType::Sizespec) {
                throw std::runtime_error("Operand of address-of operator must not use sizespec");
            }
        }
    }
}

void doCheckForAddressOf(const SymbolScope& scope, const Node& operand) {
    auto name = std::string([&]() {
        auto* id = CAST_NODE_IF_TYPE(operand, Name);
        if (id) {
            return id->literal;
        }
        auto* arrayIndexing = CAST_NODE_IF_TYPE(operand, ArrayIndexing);
        if (arrayIndexing) {
            auto* id = CAST_NODE_IF_TYPE(arrayIndexing->array, Name);
            if (id) {
                return id->literal;
            }
        }
        throw std::runtime_error("Operand of address-of operator must be identifier or array indexing expr");
    }());

    if (scope.functions.count(name) == 0) {
        auto& var = scope.variables.at(name); // must therefore be variable
        if (var.type == VariableType::Register || var.type == VariableType::Parameter) {
            throw std::runtime_error("Operand of address-of must be neither register variable or parameter");
        }
    }
}

void PrefixOperator::doAnalysis(SymbolScope scope, std::uint32_t depth) const {
    operand->doAnalysis(scope, depth);
    if (type == TokenType::And_Bit) {
        doCheckForAddressOf(scope, operand);
    } else {
        ASSERT_NAME_IS_NOT_FUNCTION_IF_NAME(operand, scope);
    }
}

BinaryOperator::BinaryOperator(TokenType type, Node operand1, Node operand2)
    : type{type}, operand1{std::move(operand1)}, operand2{std::move(operand2)} {
    if (!this->operand1 || !this->operand2) {
        throw std::runtime_error(std::format("Malformed binary operation of type  {}", tokenTypeToString(this->type)));
    }
}

void BinaryOperator::doAnalysis(SymbolScope scope, std::uint32_t depth) const {
    operand1->doAnalysis(scope, depth);
    operand2->doAnalysis(scope, depth);
    ASSERT_NAME_IS_NOT_FUNCTION_IF_NAME(operand1, scope);
    ASSERT_NAME_IS_NOT_FUNCTION_IF_NAME(operand2, scope);
}

void ExpressionStatement::doAnalysis(SymbolScope scope, std::uint32_t depth) const {
    ASSERT_NAME_IS_NOT_FUNCTION_IF_NAME(expression, scope);
    expression->doAnalysis(std::move(scope), depth);
}

Assignment::Assignment(std::optional<Token> modifyer, Node left, Node right)
    : modifyer{std::move(modifyer)}, left{std::move(left)}, right{std::move(right)} {
    if (!this->left || !this->right) {
        throw std::runtime_error("Malformed assignment statement");
    }
    if (!NODE_IS(this->left, Name) && !NODE_IS(this->left, ArrayIndexing)) {
        throw std::runtime_error("Left side of assignment must be identifier");
    }
    if (this->modifyer &&
        (this->modifyer.value().type != TokenType::Auto && this->modifyer.value().type != TokenType::Register)) {
        throw std::runtime_error("Invalid modifier for assignment");
    }
}

void Assignment::doAnalysis(SymbolScope scope, std::uint32_t depth) const {
    auto name = [&]() {
        if (NODE_IS(left, Name)) {
            return std::string(NODE_AS_REF(left, Name).literal);
        }
        auto& arr = NODE_AS_REF(left, ArrayIndexing);
        return std::string(NODE_AS_REF(arr.array, Name).literal);
    }();
    if (scope.functions.contains(name)) {
        throw std::runtime_error("Cannot declare variable with same name as function");
    }
    if (scope.variables.contains(name)) {
        auto& var = scope.variables.at(name);
        if (var.depthDecl == depth && modifyer) {
            throw std::runtime_error(
                std::format("Cannot redeclare variable {} of same name at same scope depth", name));
        }
    }
    ASSERT_NAME_IS_NOT_FUNCTION_IF_NAME(right, scope);
}

Scope::Scope(std::vector<Node> scoped) : scoped{std::move(scoped)} {
    if (std::any_of(scoped.begin(), scoped.end(), [](auto& st) { return !st; })) {
        throw std::runtime_error("Malformed scope");
    }
}

void Scope::doAnalysis(SymbolScope scope, std::uint32_t depth) const {
    for (auto& scopedStatement : scoped) {
        scopedStatement->doAnalysis(scope, depth);
        if (NODE_IS(scopedStatement, Assignment)) {
            auto& assignment = NODE_AS_REF(scopedStatement, Assignment);
            if (assignment.modifyer) { // no param -> new!
                scope.variables[std::string(NODE_AS_REF(assignment.left, Name).literal)] = {
                    .depthDecl = depth,
                    .type = (assignment.modifyer.value().type == TokenType::Register) ? VariableType::Register
                                                                                      : VariableType::Auto};
            }
        }
    }
}

If::If(Node condition, Node thenBranch, std::optional<Node> elseBranch)
    : condition{std::move(condition)}, thenBranch{std::move(thenBranch)}, elseBranch{std::move(elseBranch)} {
    if (!this->condition || !this->thenBranch || (this->elseBranch && !this->elseBranch.value())) {
        throw std::runtime_error("Malformed if");
    }
    if (!this->thenBranch->isStatement()) {
        throw std::runtime_error("Body of if then branch must be a statement");
    }
    if (!NODE_IS(this->thenBranch, Scope)) {
        if (!(NODE_IS(this->thenBranch, Assignment) || NODE_IS(this->thenBranch, Return) ||
              NODE_IS(this->thenBranch, While) || NODE_IS(this->thenBranch, If) ||
              NODE_IS(this->thenBranch, ExpressionStatement))) {
            // maybe unreachable?
            throw std::runtime_error("Body of if then branch is not among the allowed statement kinds");
        }
    }
    if (this->elseBranch) {
        auto& branch = this->elseBranch.value();
        if (!branch->isStatement()) {
            throw std::runtime_error("Body of if else branch must be a statement");
        }
        if (!NODE_IS(branch, Scope)) {
            if (!(NODE_IS(branch, Assignment) || NODE_IS(branch, Return) || NODE_IS(branch, While) ||
                  NODE_IS(branch, If) || NODE_IS(branch, ExpressionStatement))) {
                // maybe unreachable?
                throw std::runtime_error("Body of if else branch is not among the allowed statement kinds");
            }
        }
    }
}

void If::doAnalysis(SymbolScope scope, std::uint32_t depth) const {
    condition->doAnalysis(scope, depth);
    ASSERT_NAME_IS_NOT_FUNCTION_IF_NAME(condition, scope);
    thenBranch->doAnalysis(scope, depth + 1);
    if (elseBranch) {
        elseBranch.value()->doAnalysis(scope, depth + 1);
    }
}

Function::Function(Node name, std::optional<Node> argList, std::vector<Node> body)
    : name{std::move(name)}, argList{std::move(argList)}, body{std::move(body)} {
    if (!this->name || (this->argList && !this->argList.value()) ||
        std::any_of(this->body.begin(), this->body.end(), [](auto& n) { return !n; })) {
        throw std::runtime_error("Malformed function");
    }
    if (std::any_of(this->body.begin(), this->body.end(),
                    [](auto& statement) { return NODE_IS(statement, Function); })) {
        throw std::runtime_error("Function definition within function definition");
    }

    auto hasReturnStatementWithVal = [](bool nonVoid) {
        return [nonVoid](const Expression* expr) {
            if (expr->getType() == ExpressionType::Return) {
                auto* returnExpr = static_cast<const Return*>(expr);
                if (returnExpr->what) {
                    return nonVoid;
                } else {
                    return !nonVoid;
                }
            };
            return false;
        };
    };
    std::function<bool(const Expression*)> predHasVoidRet = hasReturnStatementWithVal(false);
    std::function<bool(const Expression*)> predHasNonVoidRet = hasReturnStatementWithVal(true);

    bool hasVoidRet = anyOf(predHasVoidRet);
    bool hasNonVoidRet = anyOf(predHasNonVoidRet);
    if (hasVoidRet && hasNonVoidRet) {
        throw std::runtime_error("Inconsistent return type");
    }
    isVoid = !hasNonVoidRet; // not hasVoidRet as no return statement is necessay for void funcs
}

void Function::doAnalysis(SymbolScope scope, std::uint32_t depth) const {
    if (argList) {
        const auto& parenthesised = NODE_AS_REF(argList.value(), Parenthesised).inner;
        if (NODE_IS(parenthesised, Name)) {
            const auto& parName = NODE_AS_REF(parenthesised, Name);
            ASSERT_NAME_IS_NOT_FUNCTION_IF_NAME(parenthesised, scope);
            scope.variables[std::string(parName.literal)] = {.depthDecl = depth + 1, .type = VariableType::Parameter};
        } else {
            auto& commaList = NODE_AS_REF(parenthesised, CommaList);
            if (!commaList.assertForAllElems([&](const Node& n) {
                    ASSERT_NAME_IS_NOT_FUNCTION_IF_NAME(n, scope);
                    return NODE_IS(n, Name);
                })) {
                throw std::runtime_error("Parameters must be non-function identifiers");
            }
            const auto& names = commaList.getAllNamesOnTopLevel();
            std::unordered_set<std::string_view> uniquenessChecker;
            for (auto& n : names) {
                uniquenessChecker.insert(n);
                // use the iteration to do proper bookkeeping
                scope.variables[std::string(n)] = {.depthDecl = depth + 1, .type = VariableType::Parameter};
            }
            if (uniquenessChecker.size() != names.size()) {
                throw std::runtime_error("Parameter names msut be unique");
            }
        }
    }
    for (auto& st : body) {
        st->doAnalysis(scope, depth);
        if (NODE_IS(st, Assignment)) {
            auto& assignment = NODE_AS_REF(st, Assignment);
            if (assignment.modifyer) { // no param -> new!
                scope.variables[std::string(NODE_AS_REF(assignment.left, Name).literal)] = {
                    .depthDecl = depth,
                    .type = (assignment.modifyer.value().type == TokenType::Register) ? VariableType::Register
                                                                                      : VariableType::Auto};
            }
        }
    }
    // TODO: check all paths return a value if nonvoid
}

While::While(Node cond, Node body) : condition{std::move(cond)}, body{std::move(body)} {
    if (!this->condition || !this->body) {
        throw std::runtime_error("Malformed while");
    }
    if (!this->body->isStatement()) {
        throw std::runtime_error("Body of while must be a statement");
    }
    if (!NODE_IS(this->body, Scope)) {
        if (!(NODE_IS(this->body, Assignment) || NODE_IS(this->body, Return) || NODE_IS(this->body, While) ||
              NODE_IS(this->body, If) || NODE_IS(this->body, ExpressionStatement))) {
            // maybe unreachable?
            throw std::runtime_error("Body of while is not among the allowed statement kinds");
        }
    }
}

void While::doAnalysis(SymbolScope scope, std::uint32_t depth) const {
    condition->doAnalysis(scope, depth);
    ASSERT_NAME_IS_NOT_FUNCTION_IF_NAME(condition, scope);
    body->doAnalysis(scope, depth + 1);
}

void Return::doAnalysis(SymbolScope scope, std::uint32_t depth) const {
    if (what) {
        ASSERT_NAME_IS_NOT_FUNCTION_IF_NAME(what.value(), scope);
        what.value()->doAnalysis(scope, depth);
    }
}

Return::Return(Node what) : what{std::move(what)} {
    if (this->what && !this->what.value()) {
        throw std::runtime_error("Malformed return statement.");
    }
};

FunctionCall::FunctionCall(Node name, std::optional<Node> args) : name{std::move(name)}, args{std::move(args)} {
    if (!this->name || (this->args && !this->args.value())) {
        throw std::runtime_error("Malformed function call");
    }
    if (!NODE_IS(this->name, Name)) {
        throw std::runtime_error("Function name must be unparenthesised identifier. Is: " + name->toString());
    }
}

void FunctionCall::doAnalysis(SymbolScope scope, std::uint32_t depth) const {
    name->doAnalysis(scope, depth);
    if (!scope.functions.contains(std::string(NODE_AS_REF(name, Name).literal))) {
        throw std::runtime_error("Only functions can be called");
    }
    auto argCnt = 0;
    if (args) {
        args.value()->doAnalysis(scope, depth);
        assert(NODE_IS(args.value(), Parenthesised));
        auto& list = NODE_AS_REF(args.value(), Parenthesised);
        if (NODE_IS(list.inner, CommaList))
            argCnt = NODE_AS_REF(list.inner, CommaList).getNumInList();
        else
            argCnt = 1;
    }
    if (argCnt != scope.functions.at(std::string(NODE_AS_REF(name, Name).literal)).numArgs) {
        throw std::runtime_error(
            std::format("Number of arguments of {} does not match declaration", NODE_AS_REF(name, Name).literal));
    }
}

CommaList::CommaList(Node left, Node right) : left{std::move(left)}, right{std::move(right)} {
    if (!this->left || !this->right) {
        throw std::runtime_error("Malformed comma list");
    }
}

void CommaList::doAnalysis(SymbolScope scope, std::uint32_t depth) const {
    left->doAnalysis(scope, depth);
    ASSERT_NAME_IS_NOT_FUNCTION_IF_NAME(left, scope);
    ASSERT_NAME_IS_NOT_FUNCTION_IF_NAME(right, scope);
    right->doAnalysis(std::move(scope), depth);
}
Parenthesised::Parenthesised(Node inner) : inner{std::move(inner)} {
    if (!this->inner) {
        throw std::runtime_error("Empty parenthesised expression.");
    }
}
void Parenthesised::doAnalysis(SymbolScope scope, std::uint32_t depth) const {
    inner->doAnalysis(std::move(scope), depth);
}

ArrayIndexing::ArrayIndexing(Node array, Node index) : array{std::move(array)}, index{std::move(index)} {
    // check array part?
    if (NODE_IS(this->index, BinaryOperator) && NODE_AS_REF(this->index, BinaryOperator).type == TokenType::Sizespec) {
        auto& sizespec = NODE_AS_REF(this->index, BinaryOperator);
        if (!NODE_IS(sizespec.operand2, Value)) {
            throw std::runtime_error("Sizespec must be a number!");
        }
        auto& num = NODE_AS_REF(sizespec.operand2, Value).val;
        if (num != 1 && num != 2 && num != 4 && num != 8) {
            throw std::runtime_error("Sizespec must be in {1,2,4,8}");
        }
    }
}

void ArrayIndexing::doAnalysis(SymbolScope scope, std::uint32_t depth) const {
    ASSERT_NAME_IS_NOT_FUNCTION_IF_NAME(array, scope);
    ASSERT_NAME_IS_NOT_FUNCTION_IF_NAME(index, scope);
    array->doAnalysis(scope, depth);
    index->doAnalysis(scope, depth);
}

} // namespace AST