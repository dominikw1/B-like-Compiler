#pragma once
#include "Token.h"
#include <cstdint>
#include <format>
#include <memory>
#include <string>
#include <vector>

struct Expression {
    virtual std::string toString() { return "not implemented"; }
    virtual bool isStatement() { return false; }
};

class AST {
    std::vector<std::unique_ptr<Expression>> toplevel;

  public:
    AST(std::vector<std::unique_ptr<Expression>> toplevel) : toplevel{std::move(toplevel)} {}
};

struct Value : Expression {
    std::int64_t val;
    Value(std::int64_t v) : val{v} {}
    std::string toString() override { return std::format("Value token with val {}", val); };
};

struct Name : Expression {
    std::string_view literal;
    Name(std::string_view v) : literal{v} {}
    std::string toString() override { return std::format("Name token with literal {}", literal); };
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
    std::string toString() override {
        return std::format("BinOp {} - \n\t({}) \n\t({})", tokenTypeToString(type), operand1->toString(),
                           operand2->toString());
    };
};

struct PostfixOperator : Expression {
    TokenType type;
    std::unique_ptr<Expression> operand;
    PostfixOperator(TokenType type, std::unique_ptr<Expression> operand) : type{type}, operand{std::move(operand)} {}
};

struct Statement : Expression {
    std::unique_ptr<Expression> expression;
    Statement(std::unique_ptr<Expression> expr) : expression{std::move(expr)} {}
    std::string toString() override {
        if (expression)
            return std::format("Statement: \n\t{}", expression->toString());
        return "Statement: Empty";
    }
    bool isStatement() override { return true; }
};