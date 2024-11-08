#pragma once
#include "AST.h"
#include "Token.h"
#include <format>
#include <functional>
#include <iterator>
#include <memory>
#include <span>
#include <stdexcept>
#include <unordered_map>

AST::AST parse(std::span<const Token> tokens);

namespace ParsingInternals {

enum Precedence : unsigned int;

class Parser {
    std::span<const Token> tokens;

  public:
    std::optional<AST::Node> parseExpression();
    std::optional<AST::Node> parseStatement();
    std::vector<AST::Node> parseStatements();
    AST::Node parseFunction();
    std::vector<AST::Node> parseFunctions();
    std::optional<AST::Node> parseExprWithPrecedence(std::uint32_t prec);
    std::optional<Token> consumeNextToken();
    std::optional<Token> lookaheadToken(std::uint32_t lookahead);
    bool isNextTokenOfType(TokenType type);
    Token consumeTokenOfType(TokenType type);
    Precedence getPrecedenceOfNext();
    Parser(std::span<const Token> tokens);
    AST::AST parse();
};
} // namespace ParsingInternals
