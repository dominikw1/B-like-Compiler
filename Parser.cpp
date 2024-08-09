#include "Parser.h"
#include <string>
#include <iostream>
// Heavily inspired by Crafting Interpreters by Robert Nystrom

enum Precedence : unsigned int {
    PREC_NONE,
    PREC_ASSIGNMENT,
    PREC_LOGIC_OR,
    PREC_LOGIC_AND,
    PREC_LOGIC_BIT_OR,
    PREC_LOGIC_BIT_XOR,
    PREC_LOGIC_BIT_AND,
    PREC_IS_EQUALS,
    PREC_ARITH_COMP,
    PREC_BIT_SHIFT,
    PREC_PLUS_MINUS,
    PREC_MUL_DIV_MOD,
    PREC_UNARY_OP,
    PREC_ARRAY,
};

typedef struct {
    std::unique_ptr<Expression> (*prefix)(Parser&, Token);
    std::unique_ptr<Expression> (*infix)(Parser&, std::unique_ptr<Expression>, Token);
    Precedence infixPrecedence;
} TokenParsingInfo;

std::unordered_map<TokenType, TokenParsingInfo> subParsers;

Token Parser::consumeNextToken() {
    auto val = tokens.front();
    tokens = tokens.subspan(1);
    return val;
}

Token Parser::lookaheadToken(std::uint32_t lookahead) {
    auto val = tokens[lookahead];
    tokens = tokens.subspan(1);
    return val;
}

Parser::Parser(std::span<const Token> tokens) : tokens{tokens} {}

void Parser::consumeTokenOfType(TokenType type) {
    auto nextToken = consumeNextToken();
    if (nextToken.type != type) {
        throw std::runtime_error("Expected token of type " + std::string(tokenTypeToString(type)) + ", found token " +
                                 nextToken.toString());
    }
}

[[nodiscard]]
static std::unique_ptr<Expression> parseValue(Parser& parser, Token consumed) {
    switch (consumed.type) {
    case TokenType::Number:
        return std::make_unique<Expression>(Value{std::stoll(std::string(consumed.lexeme))}); // from_chars maybe
    default:
        throw std::runtime_error("Unexpected token for rule Number " + consumed.toString());
    }
}

[[nodiscard]]
static std::unique_ptr<Expression> parseParenGroup(Parser& parser, Token consumed) {
    auto expr = parser.parseExpression();
    parser.consumeTokenOfType(TokenType::Right_Parenthesis);
    return expr;
}

[[nodiscard]]
static std::unique_ptr<Expression> parseIdentifier(Parser& parser, Token consumed) {
    return std::make_unique<Name>(consumed.lexeme);
}

[[nodiscard]]
static std::unique_ptr<Expression> parsePrefixOperator(Parser& parser, Token consumed) {
    return std::make_unique<PrefixOperator>(consumed.type, parser.parseExpression());
}

[[nodiscard]]
static std::unique_ptr<Expression> parseBinaryOperator(Parser& parser, std::unique_ptr<Expression> prev,
                                                       Token consumed) {
    return std::make_unique<BinaryOperator>(consumed.type, std::move(prev),  parser.parseExpression());
}

[[nodiscard]]
static std::unique_ptr<Expression> parsePostfixOperator(Parser& parser, std::unique_ptr<Expression> prev,
                                                        Token consumed) {
    return std::make_unique<PostfixOperator>(consumed.type, std::move(prev));
}

[[nodiscard]]
std::unique_ptr<Expression> Parser::parseExprWithPrecedence(Precedence prec) {
    Token token = consumeNextToken();
    auto prefixParser = subParsers[token.type].prefix;

    if (prefixParser == nullptr)
        throw std::runtime_error("Error parsing token " + token.toString()+". Expected an expression");

    auto parsedPrefix = prefixParser(*this, token);

    while (prec < getPrecedenceOfNext()) {
        token = consumeNextToken();
        auto infixParser = subParsers[token.type].infix;
        return infixParser(*this, std::move(parsedPrefix), token);
    }
    return parsedPrefix;
}

[[nodiscard]]
std::unique_ptr<Expression> Parser::parseExpression() {
    return parseExprWithPrecedence(Precedence::PREC_NONE);
};

Precedence Parser::getPrecedenceOfNext() {
    if (auto parser = subParsers[lookaheadToken(0).type]; parser.infix != nullptr) {
        return parser.infixPrecedence;
    }
    return Precedence::PREC_NONE;
}

void registerAllSubParsers() {
    subParsers[TokenType::Number] = {&parseValue, nullptr, Precedence::PREC_NONE};
    subParsers[TokenType::Identifier] = {&parseIdentifier, nullptr, Precedence::PREC_NONE};

    subParsers[TokenType::Minus] = {&parsePrefixOperator, &parseBinaryOperator, Precedence::PREC_PLUS_MINUS};
    subParsers[TokenType::Plus] = {&parsePrefixOperator, &parseBinaryOperator, Precedence::PREC_PLUS_MINUS};
    subParsers[TokenType::Star] = {nullptr, &parseBinaryOperator, Precedence::PREC_MUL_DIV_MOD};
    subParsers[TokenType::Slash] = {nullptr, &parseBinaryOperator, Precedence::PREC_MUL_DIV_MOD};
    subParsers[TokenType::Mod] = {nullptr, &parseBinaryOperator, Precedence::PREC_MUL_DIV_MOD};


    subParsers[TokenType::Tilde] = {&parsePrefixOperator, nullptr, Precedence::PREC_NONE};
    subParsers[TokenType::Exclamation_Mark] = {&parsePrefixOperator, nullptr, Precedence::PREC_NONE};
    subParsers[TokenType::And_Bit] = {nullptr, &parseBinaryOperator, Precedence::PREC_LOGIC_BIT_AND};
    subParsers[TokenType::And_Logical] = {nullptr, &parseBinaryOperator, Precedence::PREC_LOGIC_AND};
    subParsers[TokenType::Or_Bit] = {nullptr, &parseBinaryOperator, Precedence::PREC_LOGIC_BIT_OR};
    subParsers[TokenType::Or_Logical] = {nullptr, &parseBinaryOperator, Precedence::PREC_LOGIC_OR};

    subParsers[TokenType::Left_Parenthesis] = {&parseParenGroup, nullptr, Precedence::PREC_NONE};
}

AST Parser::parse() {
    registerAllSubParsers();
    auto firstExpr = parseExpression();
    std::cout<<firstExpr->toString()<<std::endl;
    return {};
}
