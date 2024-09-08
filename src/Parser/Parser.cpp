#include "Parser.h"
#include <iostream>
#include <string>

namespace ParsingInternals {
// Heavily inspired by Crafting Interpreters by Robert Nystrom

using namespace AST;
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
    //   std::cout << "Consumed " << val.toString() << std::endl;
    return val;
}

std::optional<Token> Parser::lookaheadToken(std::uint32_t lookahead) {
    if (tokens.size() <= lookahead)
        return {};
    return tokens[lookahead];
}

Parser::Parser(std::span<const Token> tokens) : tokens{tokens} {}

Token Parser::consumeTokenOfType(TokenType type) {
    auto nextToken = consumeNextToken();
    if (!nextToken || nextToken->type != type) {
        throw std::runtime_error("Expected token of type " + std::string(tokenTypeToString(type)) + ", found token " +
                                 nextToken->toString());
    }
    return *nextToken;
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
    auto lookahead = parser.lookaheadToken(0);
    if (lookahead && lookahead->type == TokenType::Right_Parenthesis) {
        parser.consumeTokenOfType(TokenType::Right_Parenthesis);
        return nullptr; // empty paren group - valid expr
    }
    auto expr = parser.parseExpression();
    parser.consumeTokenOfType(TokenType::Right_Parenthesis);
    return std::make_unique<Parenthesised>(std::move(expr));
}

[[nodiscard]]
static Node parseCommaList(Parser& parser, Node prev, Token _token, Precedence prec) {
    return std::make_unique<CommaList>(std::move(prev), parser.parseExprWithPrecedence(prec));
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
/*
[[nodiscard]]
static Node parsePostfixOperator(Parser& parser, Node prev, Token consumed, Precedence _) {
    return std::make_unique<PostfixOperator>(consumed.type, std::move(prev));
}*/

[[nodiscard]]
static Node parseEmptyStatement(Parser& parser, Token consumed) {
    return std::make_unique<ExpressionStatement>(nullptr);
}

[[nodiscard]]
static Node parseAssignment(Parser& parser, Token consumed) {
    // we come here through register / auto
    // std::cout << "In correct function\n";
    auto left = parser.parseExprWithPrecedence(
        Precedence::PREC_LOGIC_OR); // to make sure we are not accidentally picking up the whole assignemt
    parser.consumeTokenOfType(TokenType::Assignment);
    auto right = parser.parseExpression();
    parser.consumeTokenOfType(TokenType::Semicolon);
    return std::make_unique<Assignment>(consumed, std::move(left), std::move(right));
}

[[nodiscard]]
static Node parseAssignment(Parser& parser, Node prev, Token consumed, Precedence prec) {
    // std::cout << "in correct method" << std::endl;
    auto right = parser.parseExpression();
    parser.consumeTokenOfType(TokenType::Semicolon);
    return std::make_unique<Assignment>(std::move(prev), std::move(right));
}

[[nodiscard]]
static Node parseScope(Parser& parser, Token consumed) {
    // std::cout << "Tryna parse scope " << std::endl;
    auto ret = std::make_unique<Scope>(parser.parseStatements());
    parser.consumeTokenOfType(TokenType::Right_Brace);
    return ret;
}

[[nodiscard]]
static Node parseFunctionCall(Parser& parser, Node prev, Token consumed, Precedence _prec) {
    auto args = parseParenGroup(parser, std::move(consumed));
    return std::make_unique<FunctionCall>(std::move(prev), args ? std::optional{std::move(args)} : std::nullopt);
}

[[nodiscard]]
static Node parseIf(Parser& parser, Token consumed) {
    parser.consumeTokenOfType(TokenType::Left_Parenthesis);
    auto cond = parser.parseExpression();
    parser.consumeTokenOfType(TokenType::Right_Parenthesis);
    auto thenBranch = parser.parseStatement();

    auto elseBranch = [&parser]() -> std::optional<Node> {
        if (parser.isNextTokenOfType(TokenType::Else)) {
            parser.consumeTokenOfType(TokenType::Else);
            return std::optional(parser.parseStatement());
        }
        return std::nullopt;
    }();

    return std::make_unique<If>(std::move(cond), std::move(thenBranch), std::move(elseBranch));
}

[[nodiscard]]
static Node parseWhile(Parser& parser, Token consumed) {
    parser.consumeTokenOfType(TokenType::Left_Parenthesis);
    auto cond = parser.parseExpression();
    parser.consumeTokenOfType(TokenType::Right_Parenthesis);
    auto body = parser.parseStatement();
    return std::make_unique<While>(std::move(cond), std::move(body));
}

[[nodiscard]]
static Node parseArrayIndexing(Parser& parser, Node prev, Token _consumed, Precedence prec) {
    auto index = parser.parseExpression(); // not using prec cause otherwise nothing will be found
    parser.consumeTokenOfType(TokenType::Right_Bracket);
    return std::make_unique<ArrayIndexing>(std::move(prev), std::move(index));
}

[[nodiscard]]
static Node parseReturn(Parser& parser, Token consumed) {
    if (auto next = parser.lookaheadToken(0); next && next->type == TokenType::Semicolon) {
        parser.consumeTokenOfType(TokenType::Semicolon);
        return std::make_unique<Return>();
    }
    auto what = parser.parseExpression();
    parser.consumeTokenOfType(TokenType::Semicolon);
    return std::make_unique<Return>(std::move(what));
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
std::vector<Node> Parser::parseStatements() {
    std::vector<Node> statements;
    for (auto statement = parseStatement(); statement; statement = parseStatement()) {
        statements.push_back(std::move(statement));
    }
    return statements;
}

[[nodiscard]]
Node Parser::parseStatement() {
    // std::cout << "tryna parse statement" << std::endl;
    if (auto lookahead = lookaheadToken(0); lookahead && lookahead->type == TokenType::Right_Brace) {
        return nullptr; // end of scope
    }
    auto expr = parseExprWithPrecedence(Precedence::PREC_ASSIGNMENT);
    if (!expr)
        return nullptr;
    if (expr->isStatement())
        return expr;
    consumeTokenOfType(TokenType::Semicolon);
    return std::make_unique<ExpressionStatement>(std::move(expr));
}

[[nodiscard]]
Node Parser::parseFunction() {
    if (tokens.size() == 0)
        return nullptr;
    auto name = parseIdentifier(*this, consumeTokenOfType(TokenType::Identifier));
    auto argList = parseParenGroup(*this, consumeTokenOfType(TokenType::Left_Parenthesis));
    consumeTokenOfType(TokenType::Left_Brace);
    auto statements = parseStatements();
    consumeTokenOfType(TokenType::Right_Brace);
    return std::make_unique<Function>(std::move(name), argList ? std::optional{std::move(argList)} : std::nullopt,
                                      std::move(statements));
}

[[nodiscard]]
std::vector<Node> Parser::parseFunctions() {
    std::vector<Node> functions;
    for (auto function = parseFunction(); function; function = parseFunction()) {
        // std::cout << function->toString() << std::endl;
        functions.push_back(std::move(function));
        //std::cout << functions.back()->toString() << std::endl;
    }
    return functions;
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
    subParsers[TokenType::Sizespec] = {nullptr, &parseBinaryOperator, PREC_ASSIGNMENT};

    subParsers[TokenType::Tilde] = {&parsePrefixOperator, nullptr, Precedence::PREC_NONE};
    subParsers[TokenType::And_Bit] = {&parsePrefixOperator, &parseBinaryOperator, Precedence::PREC_LOGIC_BIT_AND};
    subParsers[TokenType::Or_Bit] = {nullptr, &parseBinaryOperator, Precedence::PREC_LOGIC_BIT_OR};
    subParsers[TokenType::Xor] = {nullptr, &parseBinaryOperator, PREC_LOGIC_BIT_XOR};
    subParsers[TokenType::Bitshift_Left] = {nullptr, &parseBinaryOperator, PREC_BIT_SHIFT};
    subParsers[TokenType::Bitshift_Right] = {nullptr, &parseBinaryOperator, PREC_BIT_SHIFT};

    subParsers[TokenType::And_Logical] = {nullptr, &parseBinaryOperator, Precedence::PREC_LOGIC_AND};
    subParsers[TokenType::Or_Logical] = {nullptr, &parseBinaryOperator, Precedence::PREC_LOGIC_OR};

    subParsers[TokenType::Left_Parenthesis] = {&parseParenGroup, &parseFunctionCall, Precedence::PREC_ARRAY};
    subParsers[TokenType::Right_Parenthesis] = {nullptr, nullptr, Precedence::PREC_NONE};
    subParsers[TokenType::Left_Brace] = {&parseScope, nullptr, PREC_NONE};
    subParsers[TokenType::Right_Brace] = {nullptr, nullptr, PREC_NONE};
    subParsers[TokenType::Left_Bracket] = {nullptr, &parseArrayIndexing, PREC_ARRAY};
    subParsers[TokenType::Right_Bracket] = {nullptr, nullptr, PREC_NONE};

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
    subParsers[TokenType::Register] = {&parseAssignment, nullptr, PREC_NONE};
    subParsers[TokenType::Auto] = {&parseAssignment, nullptr, PREC_NONE};
    subParsers[TokenType::While] = {&parseWhile, nullptr, PREC_NONE};
    subParsers[TokenType::Return] = {&parseReturn, nullptr, PREC_NONE};
    subParsers[TokenType::Comma] = {nullptr, &parseCommaList, PREC_ASSIGNMENT};
}

AST::AST Parser::parse() {
    //std::cout << "Starting parsing..." << std::endl;
    registerAllSubParsers();
    auto funcs = parseFunctions();
    if (tokens.size() != 0) {
        throw std::runtime_error("Malformed program");
    }
    return AST::AST{std::move(funcs)};
}

} // namespace ParsingInternals

AST::AST parse(std::span<const Token> tokens) {
    ParsingInternals::Parser p{tokens};
    auto ast = p.parse();
    ast.analyze();
    return ast;
}
