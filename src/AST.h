#pragma once
#include "Token.h"
#include <cstdint>
#include <format>
#include <memory>
#include <string>
#include <vector>

enum class ExpressionType {
    Value,
    Name,
    PrefixOperator,
    BinaryOperator,
    PostfixOperator,
    ExpressionStatement,
    Assignment,
    Scope,
    If
};

#define NODE_AS_PTR(node, TYPE) (static_cast<const TYPE*>(node.get()))
#define NODE_AS_REF(node, TYPE) (*NODE_AS_PTR(node, TYPE))
#define NODE_IS(node, TYPE) (node->getType() == ExpressionType::TYPE)
#define CAST_NODE_IF_TYPE(node, TYPE) (NODE_IS(node, TYPE) ? NODE_AS_PTR(node, TYPE) : nullptr);

struct Expression {
    virtual std::string toString() const { return "not implemented"; }
    virtual bool isStatement() const { return false; }
    constexpr virtual ExpressionType getType() const = 0;
};

using Node = std::unique_ptr<Expression>;

class AST {
    std::vector<Node> toplevel;

  public:
    AST(std::vector<Node> toplevel) : toplevel{std::move(toplevel)} {}
    const std::vector<Node>& getTopLevel() const { return toplevel; };
};

struct Value : Expression {
    std::int64_t val;
    Value(std::int64_t v) : val{v} {}
    std::string toString() const override { return std::format("Value token with val {}", val); };
    constexpr ExpressionType getType() const override { return ExpressionType::Value; }
};

struct Name : Expression {
    std::string_view literal;
    Name(std::string_view v) : literal{v} {}
    std::string toString() const override { return std::format("Name token with literal {}", literal); };
    constexpr ExpressionType getType() const override { return ExpressionType::Name; }
};

struct PrefixOperator : Expression {
    TokenType type;
    Node operand;
    PrefixOperator(TokenType type, Node operand) : type{type}, operand{std::move(operand)} {}
    constexpr ExpressionType getType() const override { return ExpressionType::PrefixOperator; }
};

struct BinaryOperator : Expression {
    TokenType type;
    Node operand1;
    Node operand2;
    BinaryOperator(TokenType type, Node operand1, Node operand2)
        : type{type}, operand1{std::move(operand1)}, operand2{std::move(operand2)} {}
    std::string toString() const override {
        return std::format("BinOp {} - \n\t({}) \n\t({})", tokenTypeToString(type), operand1->toString(),
                           operand2->toString());
    };
    constexpr ExpressionType getType() const override { return ExpressionType::BinaryOperator; }
};

struct PostfixOperator : Expression {
    TokenType type;
    Node operand;
    PostfixOperator(TokenType type, Node operand) : type{type}, operand{std::move(operand)} {}
    constexpr ExpressionType getType() const override { return ExpressionType::PostfixOperator; }
};

struct Statement : Expression {
    bool isStatement() const override { return true; }
};

struct ExpressionStatement : Statement {
    Node expression;
    ExpressionStatement(Node expr) : expression{std::move(expr)} {}
    std::string toString() const override {
        if (expression)
            return std::format("Statement: \n\t{}", expression->toString());
        return "Statement: Empty";
    }
    constexpr ExpressionType getType() const override { return ExpressionType::ExpressionStatement; }
};

struct Assignment : Expression {
    Node left;
    Node right;
    Assignment(Node left, Node right) : left{std::move(left)}, right{std::move(right)} {}
    std::string toString() const override {
        return std::format("Assignment: \n\t{} = {}", left->toString(), right->toString());
    }
    constexpr ExpressionType getType() const override { return ExpressionType::Assignment; }
};

struct Scope : Statement {
    Node scoped;
    Scope(Node scoped) : scoped{std::move(scoped)} {}
    std::string toString() const override { return "{" + scoped->toString() + "}"; }
    constexpr ExpressionType getType() const override { return ExpressionType::Scope; }
};

struct If : Statement {
    Node condition;
    Node thenBranch;
    Node elseBranch;
    If(Node condition, Node thenBranch, Node elseBranch = nullptr)
        : condition{std::move(condition)}, thenBranch{std::move(thenBranch)}, elseBranch{std::move(elseBranch)} {}
    std::string toString() const override {
        return std::format("If {} then {} {}", condition->toString(), thenBranch->toString(),
                           std::format("{}", elseBranch ? elseBranch->toString() : "[no else]"));
    }
    constexpr ExpressionType getType() const override { return ExpressionType::If; }
};