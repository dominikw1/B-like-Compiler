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

using Node = std::unique_ptr<Expression>;

class AST {
    std::vector<Node> toplevel;

  public:
    AST(std::vector<Node> toplevel) : toplevel{std::move(toplevel)} {}
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
    Node operand;
    PrefixOperator(TokenType type, Node operand) : type{type}, operand{std::move(operand)} {}
};

struct BinaryOperator : Expression {
    TokenType type;
    Node operand1;
    Node operand2;
    BinaryOperator(TokenType type, Node operand1, Node operand2)
        : type{type}, operand1{std::move(operand1)}, operand2{std::move(operand2)} {}
    std::string toString() override {
        return std::format("BinOp {} - \n\t({}) \n\t({})", tokenTypeToString(type), operand1->toString(),
                           operand2->toString());
    };
};

struct PostfixOperator : Expression {
    TokenType type;
    Node operand;
    PostfixOperator(TokenType type, Node operand) : type{type}, operand{std::move(operand)} {}
};

struct Statement : Expression {
    bool isStatement() override { return true; }
};

struct ExpressionStatement : Statement {
    Node expression;
    ExpressionStatement(Node expr) : expression{std::move(expr)} {}
    std::string toString() override {
        if (expression)
            return std::format("Statement: \n\t{}", expression->toString());
        return "Statement: Empty";
    }
};

struct Assignment : Expression {
    Node left;
    Node right;
    Assignment(Node left, Node right) : left{std::move(left)}, right{std::move(right)} {}
    std::string toString() override {
        return std::format("Assignment: \n\t{} = {}", left->toString(), right->toString());
    }
};

struct Scope : Statement {
    Node scoped;
    Scope(Node scoped) : scoped{std::move(scoped)} {}
    std::string toString() override { return "{" + scoped->toString() + "}"; }
};

struct If : Statement {
    Node condition;
    Node thenBranch;
    Node elseBranch;
    If(Node condition, Node thenBranch, Node elseBranch = nullptr)
        : condition{std::move(condition)}, thenBranch{std::move(thenBranch)}, elseBranch{std::move(elseBranch)} {}
    std::string toString() override {
        return std::format("If {} then {} else {}", condition->toString(), thenBranch->toString(),
                           elseBranch->toString());
    }
};