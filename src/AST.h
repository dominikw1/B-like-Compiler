#pragma once
#include "Symbols.h"
#include "Token.h"
#include <algorithm>
#include <cassert>
#include <cstdint>
#include <format>
#include <iostream>
#include <memory>
#include <string>
#include <vector>

enum class ExpressionType {
    Value,
    Name,
    PrefixOperator,
    BinaryOperator,
    ExpressionStatement,
    Assignment,
    Scope,
    If,
    Function,
    While,
    Return,
    FunctionCall,
    CommaList,
    ArrayIndexing,
    Parenthesised,
};

#define NODE_AS_PTR(node, TYPE) (static_cast<const TYPE*>(node.get()))
#define NODE_AS_REF(node, TYPE) (*NODE_AS_PTR(node, TYPE))
#define NODE_IS(node, TYPE) (node->getType() == ExpressionType::TYPE)
#define CAST_NODE_IF_TYPE(node, TYPE) (NODE_IS(node, TYPE) ? NODE_AS_PTR(node, TYPE) : (TYPE*)nullptr);

struct Expression {
    virtual std::string toString() const { return "not implemented"; }
    virtual bool isStatement() const { return false; }
    virtual void doAnalysis(SymbolScope scope, std::uint32_t depth) const = 0;
    constexpr virtual ExpressionType getType() const = 0;
};

using Node = std::unique_ptr<Expression>;

class AST {
    std::vector<Node> toplevel;

  public:
    AST(std::vector<Node> toplevel) : toplevel{std::move(toplevel)} {}
    const std::vector<Node>& getTopLevel() const { return toplevel; };
    Node take(size_t index) { return std::move(toplevel[index]); };
};

struct Value : Expression {
    std::int64_t val;
    Value(std::int64_t v) : val{v} {}
    std::string toString() const override { return std::format("Value token with val {}", val); };
    constexpr ExpressionType getType() const override { return ExpressionType::Value; }
    void doAnalysis(SymbolScope scope, std::uint32_t depth) const;
};

struct Name : Expression {
    std::string_view literal;
    Name(std::string_view v) : literal{v} {}
    std::string toString() const override { return std::format("Name token with literal {}", literal); };
    constexpr ExpressionType getType() const override { return ExpressionType::Name; }
    void doAnalysis(SymbolScope scope, std::uint32_t depth) const;
};

struct PrefixOperator : Expression {
    TokenType type;
    Node operand;
    PrefixOperator(TokenType type, Node operand);
    constexpr ExpressionType getType() const override { return ExpressionType::PrefixOperator; }
    void doAnalysis(SymbolScope scope, std::uint32_t depth) const;
};

struct BinaryOperator : Expression {
    TokenType type;
    Node operand1;
    Node operand2;
    BinaryOperator(TokenType type, Node operand1, Node operand2);
    std::string toString() const override {
        return std::format("BinOp {} - \n\t({}) \n\t({})", tokenTypeToString(type), operand1->toString(),
                           operand2->toString());
    };
    constexpr ExpressionType getType() const override { return ExpressionType::BinaryOperator; }
    void doAnalysis(SymbolScope scope, std::uint32_t depth) const;
};
/**
struct PostfixOperator : Expression {
    TokenType type;
    Node operand;
    PostfixOperator(TokenType type, Node operand) : type{type}, operand{std::move(operand)} {}
    constexpr ExpressionType getType() const override { return ExpressionType::PostfixOperator; }
};
*/
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
    void doAnalysis(SymbolScope scope, std::uint32_t depth) const;
};

struct Assignment : Statement {
    std::optional<Token> modifyer{};
    Node left;
    Node right;
    Assignment(std::optional<Token> modifyer, Node left, Node right);
    Assignment(Node left, Node right) : Assignment(std::nullopt, std::move(left), std::move(right)) {}
    std::string toString() const override {
        return std::format("Assignment: \n\t{} {} = {}", modifyer ? modifyer->lexeme : "", left->toString(),
                           right->toString());
    }
    constexpr ExpressionType getType() const override { return ExpressionType::Assignment; }
    void doAnalysis(SymbolScope scope, std::uint32_t depth) const;
};

struct Scope : Statement {
    std::vector<Node> scoped;
    Scope(std::vector<Node> scoped);
    std::string toString() const override {
        std::string retVal = "{\n";
        for (auto& s : scoped) {
            retVal += s->toString() + "\n";
        }
        return retVal + "}\n";
    }
    constexpr ExpressionType getType() const override { return ExpressionType::Scope; }
    void doAnalysis(SymbolScope scope, std::uint32_t depth) const;
};

struct If : Statement {
    Node condition;
    Node thenBranch;
    std::optional<Node> elseBranch{std::nullopt};
    If(Node condition, Node thenBranch, std::optional<Node> elseBranch = std::nullopt);
    std::string toString() const override {
        return std::format("If {} then {} {}", condition->toString(), thenBranch->toString(),
                           std::format("{}", elseBranch ? elseBranch.value()->toString() : "[no else]"));
    }
    constexpr ExpressionType getType() const override { return ExpressionType::If; }
    void doAnalysis(SymbolScope scope, std::uint32_t depth) const;
};

struct Function : Statement {
    Node name;
    std::optional<Node> argList{std::nullopt};
    std::vector<Node> body;
    Function(Node name, std::optional<Node> argList, std::vector<Node> body);

    std::string toString() const override {
        auto header = std::format("Function {} ({})\n", name->toString(), argList ? argList.value()->toString() : "");
        for (auto& s : body) {
            assert(s);
            header += s->toString() + "\n";
        }
        header += "\n";
        return header;
    }
    constexpr ExpressionType getType() const override { return ExpressionType::Function; }
    void doAnalysis(SymbolScope scope, std::uint32_t depth) const;
};

struct While : Statement {
    Node condition;
    Node body;

    While(Node cond, Node body);

    std::string toString() const override {
        return std::format("while ({})  {}  \n", condition->toString(), body->toString()) + "}\n";
    }
    constexpr ExpressionType getType() const override { return ExpressionType::While; }
    void doAnalysis(SymbolScope scope, std::uint32_t depth) const;
};

struct Return : Statement {
    std::optional<Node> what{};

    Return() {}
    Return(Node what);

    std::string toString() const override { return std::format("return {}", what ? (*what)->toString() : ""); }
    constexpr ExpressionType getType() const override { return ExpressionType::Return; }
    void doAnalysis(SymbolScope scope, std::uint32_t depth) const;
};

struct FunctionCall : Expression {
    Node name;
    std::optional<Node> args{std::nullopt};

    FunctionCall(Node name, std::optional<Node> args);
    std::string toString() const override {
        return std::format("calling function {} with args ({})", name->toString(),
                           args ? args.value()->toString() : "");
    }
    constexpr ExpressionType getType() const override { return ExpressionType::FunctionCall; }
    void doAnalysis(SymbolScope scope, std::uint32_t depth) const;
};

struct CommaList : Expression {
    Node left;
    Node right; // either Comma list or type

    CommaList(Node left, Node right);
    std::string toString() const override { return std::format("{}, {}", left->toString(), right->toString()); }
    constexpr ExpressionType getType() const override { return ExpressionType::CommaList; }
    void doAnalysis(SymbolScope scope, std::uint32_t depth) const;
};

struct Parenthesised : Expression {
    Node inner;
    Parenthesised(Node inner);
    std::string toString() const override { return std::format("( {} )", inner->toString()); }
    constexpr ExpressionType getType() const override { return ExpressionType::Parenthesised; }
    void doAnalysis(SymbolScope scope, std::uint32_t depth) const;
};

struct ArrayIndexing : Expression {
    Node array;
    Node index;

    ArrayIndexing(Node array, Node index);
    std::string toString() const override { return std::format("{}[{}]", array->toString(), index->toString()); }
    constexpr ExpressionType getType() const override { return ExpressionType::ArrayIndexing; }
    void doAnalysis(SymbolScope scope, std::uint32_t depth) const;
};