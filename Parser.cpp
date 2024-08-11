#include "Parser.h"
#include <iostream>
#include <string>
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
    Node (*prefix)(Parser&, Token);
    Node (*infix)(Parser&, Node, Token, Precedence);
    Precedence infixPrecedence;
} TokenParsingInfo;

std::unordered_map<TokenType, TokenParsingInfo> subParsers;

std::optional<Token> Parser::consumeNextToken() {
    if (tokens.size() < 1)
        return {};
    auto val = tokens.front();
    tokens = tokens.subspan(1);
    std::cout<<"Consumed "<<val.toString()<<std::endl;
    return val;
}

std::optional<Token> Parser::lookaheadToken(std::uint32_t lookahead) {
    if (tokens.size() < lookahead)
        return {};
    return tokens[lookahead];
}

Parser::Parser(std::span<const Token> tokens) : tokens{tokens} {}

void Parser::consumeTokenOfType(TokenType type) {
    auto nextToken = consumeNextToken();
    if (!nextToken || nextToken->type != type) {
        throw std::runtime_error("Expected token of type " + std::string(tokenTypeToString(type)) + ", found token " +
                                 nextToken->toString());
    }
}

bool Parser::isNextTokenOfType(TokenType type) {
    if (auto nextToken = lookaheadToken(0)) {
        return nextToken->type == type;
    }
    return false;
}

[[nodiscard]]
static Node parseValue(Parser& parser, Token consumed) {
    return std::make_unique<Value>(std::stoll(std::string(consumed.lexeme))); // from_chars maybe
}

[[nodiscard]]
static Node parseParenGroup(Parser& parser, Token consumed) {
    auto expr = parser.parseExpression();
    parser.consumeTokenOfType(TokenType::Right_Parenthesis);
    return expr;
}

[[nodiscard]]
static Node parseIdentifier(Parser& parser, Token consumed) {
    return std::make_unique<Name>(consumed.lexeme);
}

[[nodiscard]]
static Node parsePrefixOperator(Parser& parser, Token consumed) {
    return std::make_unique<PrefixOperator>(consumed.type, parser.parseExprWithPrecedence(PREC_UNARY_OP));
}

[[nodiscard]]
static Node parseBinaryOperator(Parser& parser, Node prev, Token consumed, Precedence currPrec) {
    return std::make_unique<BinaryOperator>(consumed.type, std::move(prev), parser.parseExprWithPrecedence(currPrec));
}

[[nodiscard]]
static Node parsePostfixOperator(Parser& parser, Node prev, Token consumed, Precedence _) {
    return std::make_unique<PostfixOperator>(consumed.type, std::move(prev));
}

[[nodiscard]]
static Node parseEmptyStatement(Parser& parser, Token consumed) {
    return std::make_unique<ExpressionStatement>(nullptr);
}

[[nodiscard]]
static Node parseAssignment(Parser& parser, Node prev, Token consumed, Precedence _) {
    return std::make_unique<Assignment>(std::move(prev), parser.parseExpression());
}

[[nodiscard]]
static Node parseScope(Parser& parser, Token consumed) {
    auto ret = std::make_unique<Scope>(parser.parseExpression());
    parser.consumeTokenOfType(TokenType::Right_Brace);
    return ret;
}

[[nodiscard]]
static Node parseIf(Parser& parser, Token consumed) {
    std::cout << "parsing if" << std::endl;
    parser.consumeTokenOfType(TokenType::Left_Parenthesis);
    auto cond = parser.parseExpression();
    parser.consumeTokenOfType(TokenType::Right_Parenthesis);
    auto thenBranch = [&parser]() -> Node {
        if (parser.isNextTokenOfType(TokenType::Left_Brace)) {
            return parseScope(parser, *parser.consumeNextToken());
        }
        return std::make_unique<Scope>(parser.parseStatement());
    }();
    auto elseBranch = [&parser]() -> Node {
        if (parser.isNextTokenOfType(TokenType::Else)) {
            if (parser.isNextTokenOfType(TokenType::Left_Brace)) {
                return parseScope(parser, *parser.consumeNextToken());
            }
            return std::make_unique<Scope>(parser.parseStatement());
        }
        return nullptr;
    }();
    return std::make_unique<If>(std::move(cond), std::move(thenBranch), std::move(elseBranch));
}

[[nodiscard]]
Node Parser::parseExprWithPrecedence(Precedence prec) {
    auto maybeToken = consumeNextToken();
    if (!maybeToken) {
        return nullptr;
    }
    Token token = *maybeToken;

    auto prefixParser = subParsers[token.type].prefix;

    if (prefixParser == nullptr)
        throw std::runtime_error("Error parsing token " + token.toString() + ". Expected an expression");

    auto parsedPrefix = prefixParser(*this, token);
    // std::cout << "curr prec = " << prec << std::endl;
    while (prec <= getPrecedenceOfNext()) {
        maybeToken = consumeNextToken();
        if (!maybeToken) {
            return parsedPrefix;
        }
        token = *maybeToken;
        auto infixParser = subParsers[token.type].infix;
        parsedPrefix = infixParser(*this, std::move(parsedPrefix), token, subParsers[token.type].infixPrecedence);
    }
    return parsedPrefix;
}

[[nodiscard]]
Node Parser::parseExpression() {
    return parseExprWithPrecedence(Precedence::PREC_ASSIGNMENT);
};

[[nodiscard]]
Node Parser::parseStatement() {
    auto expr = parseExprWithPrecedence(Precedence::PREC_ASSIGNMENT);
    if (!expr)
        return nullptr;
    if (expr->isStatement())
        return expr;
    consumeTokenOfType(TokenType::Semicolon);
    return std::make_unique<ExpressionStatement>(std::move(expr));
}

[[nodiscard]]
Precedence Parser::getPrecedenceOfNext() {
    auto nextToken = lookaheadToken(0);
    if (!nextToken) {
        return Precedence::PREC_NONE;
    }
    // std::cout << "Taking a lookahead at " << nextToken->toString() << std::endl;
    if (auto parser = subParsers.at(nextToken->type); parser.infix != nullptr) {
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

    subParsers[TokenType::Exclamation_Mark] = {&parsePrefixOperator, nullptr, Precedence::PREC_NONE};

    subParsers[TokenType::Tilde] = {&parsePrefixOperator, nullptr, Precedence::PREC_NONE};
    subParsers[TokenType::And_Bit] = {nullptr, &parseBinaryOperator, Precedence::PREC_LOGIC_BIT_AND};
    subParsers[TokenType::Or_Bit] = {nullptr, &parseBinaryOperator, Precedence::PREC_LOGIC_BIT_OR};
    subParsers[TokenType::Xor] = {nullptr, &parseBinaryOperator, PREC_LOGIC_BIT_XOR};

    subParsers[TokenType::And_Logical] = {nullptr, &parseBinaryOperator, Precedence::PREC_LOGIC_AND};
    subParsers[TokenType::Or_Logical] = {nullptr, &parseBinaryOperator, Precedence::PREC_LOGIC_OR};

    subParsers[TokenType::Left_Parenthesis] = {&parseParenGroup, nullptr, Precedence::PREC_NONE};
    subParsers[TokenType::Right_Parenthesis] = {nullptr, nullptr, Precedence::PREC_NONE};
    subParsers[TokenType::Left_Brace] = {&parseScope, nullptr, PREC_NONE};
    subParsers[TokenType::Right_Brace] = {nullptr, nullptr, PREC_NONE};

    subParsers[TokenType::Semicolon] = {&parseEmptyStatement, nullptr, Precedence::PREC_NONE};
    subParsers[TokenType::Assignment] = {nullptr, &parseAssignment, PREC_ASSIGNMENT};
    // Comparisons
    subParsers[TokenType::Equals] = {nullptr, &parseBinaryOperator, PREC_IS_EQUALS};
    subParsers[TokenType::Uneqal] = {nullptr, &parseBinaryOperator, PREC_IS_EQUALS};
    subParsers[TokenType::Larger] = {nullptr, &parseBinaryOperator, PREC_IS_EQUALS};
    subParsers[TokenType::Larger_Equal] = {nullptr, &parseBinaryOperator, PREC_IS_EQUALS};
    subParsers[TokenType::Smaller] = {nullptr, &parseBinaryOperator, PREC_IS_EQUALS};
    subParsers[TokenType::Smaller_Equal] = {nullptr, &parseBinaryOperator, PREC_IS_EQUALS};

    // Keywords
    subParsers[TokenType::If] = {&parseIf, nullptr, PREC_NONE};
    subParsers[TokenType::Else] = {nullptr, nullptr, PREC_NONE};
}

AST Parser::parse() {
    std::cout << "Starting parsing..." << std::endl;
    registerAllSubParsers();
    std::vector<Node> toplevel;
    for (auto statement = parseStatement(); statement; statement = parseStatement()) {
        std::cout << statement->toString() << std::endl;
        toplevel.push_back(std::move(statement));
    }
    return AST{std::move(toplevel)};
}
