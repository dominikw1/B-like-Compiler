#pragma once
#include "Token.h"
#include <cstdint>
#include <memory>
#include <string>
#include <vector>

class AST {};

struct Expression {
    virtual std::string toString() { return "not implemented"; }
};

struct ConditionalExpression : Expression {
    std::unique_ptr<Expression> condition;
    std::unique_ptr<Expression> thenBranch;
    std::unique_ptr<Expression> elseBranch;
};

struct Value : Expression {
    std::int64_t val;
    Value(std::int64_t v) : val{v} {}
};

struct Name : Expression {
    std::string_view literal;
    Name(std::string_view v) : literal{v} {}
};

struct PrefixOperator : Expression {
    TokenType type;
    std::unique_ptr<Expression> operand;
    PrefixOperator(TokenType type, std::unique_ptr<Expression> operand) : type{type}, operand{std::move(operand)} {}
};

struct BinaryOperator : Expression {
    TokenType type;
    std::unique_ptr<Expression> operand1;
    std::unique_ptr<Expression> operand2;
    BinaryOperator(TokenType type, std::unique_ptr<Expression> operand1, std::unique_ptr<Expression> operand2)
        : type{type}, operand1{std::move(operand1)}, operand2{std::move(operand2)} {}
};

struct PostfixOperator : Expression {
    TokenType type;
    std::unique_ptr<Expression> operand;
    PostfixOperator(TokenType type, std::unique_ptr<Expression> operand) : type{type}, operand{std::move(operand)} {}
};
