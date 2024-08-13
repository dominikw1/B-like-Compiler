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

enum Precedence : unsigned int;

class Parser {

    // void assertNextTokenIsOfType(TokenType type, auto tokens) {
    //     if (auto nextToken = getNextToken(tokens); nextToken.type != type) {
    //         throw std::runtime_error(std::format("Expected token type {}, found type {}", tokenTypeToString(type),
    //                                              tokenTypeToString(nextToken.type)));
    //     }
    // }

    std::span<const Token> tokens;

  public:
    Node parseExpression();
    Node parseStatement();
    std::vector<Node> parseStatements();
    Node parseFunction();
    std::vector<Node> parseFunctions();
    Node parseExprWithPrecedence(Precedence prec);
    std::optional<Token> consumeNextToken();
    std::optional<Token> lookaheadToken(std::uint32_t lookahead);
    bool isNextTokenOfType(TokenType type);
    Token consumeTokenOfType(TokenType type);
    Precedence getPrecedenceOfNext();
    Parser(std::span<const Token> tokens);
    AST parse();
};
