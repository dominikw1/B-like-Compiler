#include "Parser.h"
#include <iostream>
#include <string>

namespace ParsingInternals {

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

enum class Associativity { Left, Right, None };

struct TokenParsingInfo {
    Node (*prefix)(Parser&, Token);
    Node (*infix)(Parser&, Node, Token, std::uint32_t);
    Precedence infixPrecedence;
    Associativity associativity;
};

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
    std::int64_t result;
    auto [ptr, ec] = std::from_chars(consumed.lexeme.data(), consumed.lexeme.data() + consumed.lexeme.size(), result);
    return std::make_unique<Value>(result);
}

[[nodiscard]]
static Node parseParenGroup(Parser& parser, Token consumed) {
    auto lookahead = parser.lookaheadToken(0);
    if (lookahead && lookahead->type == TokenType::Right_Parenthesis) {
        parser.consumeTokenOfType(TokenType::Right_Parenthesis);
        return std::make_unique<Parenthesised>(std::nullopt); // empty paren group - valid expr
    }
    auto expr = parser.parseExpression();
    parser.consumeTokenOfType(TokenType::Right_Parenthesis);
    return std::make_unique<Parenthesised>(std::move(expr));
}

[[nodiscard]]
static Node parseCommaList(Parser& parser, Node prev, Token _token, std::uint32_t prec) {
    auto right = parser.parseExprWithPrecedence(prec);
    if (!right) {
        throw std::runtime_error("comma list incomplete");
    }
    return std::make_unique<CommaList>(std::move(prev), std::move(right.value()));
}

[[nodiscard]]
static Node parseIdentifier(Parser& parser, Token consumed) {
    return std::make_unique<Name>(consumed.lexeme);
}

[[nodiscard]]
static Node parsePrefixOperator(Parser& parser, Token consumed) {
    auto exp = parser.parseExprWithPrecedence(PREC_UNARY_OP);
    if (!exp) {
        throw std::runtime_error("Incomplete unary op");
    }
    return std::make_unique<PrefixOperator>(consumed.type, std::move(exp.value()));
}

[[nodiscard]]
static Node parseBinaryOperator(Parser& parser, Node prev, Token consumed, std::uint32_t currPrec) {
    auto exp = parser.parseExprWithPrecedence(currPrec);
    if (!exp) {
        throw std::runtime_error("Incomplete binary op");
    }
    return std::make_unique<BinaryOperator>(consumed.type, std::move(prev), std::move(exp.value()));
}

[[nodiscard]]
static Node parseEmptyStatement(Parser& parser, Token consumed) {
    return std::make_unique<ExpressionStatement>(nullptr);
}

[[nodiscard]]
static Node parseAssignment(Parser& parser, Token consumed) {
    // we come here through register / auto
    auto left = parser.parseExprWithPrecedence(
        Precedence::PREC_LOGIC_OR); // to make sure we are not accidentally picking up the whole assignemt
    if (!left) {
        throw std::runtime_error("Failed to parse left side of assignment");
    }
    parser.consumeTokenOfType(TokenType::Assignment);
    //   std::cout << "parsing assignment decl\n";
    auto right = parser.parseExpression();
    if (!right) {
        throw std::runtime_error("Failed to parse right side of assignment");
    }
    parser.consumeTokenOfType(TokenType::Semicolon);
    return std::make_unique<Assignment>(consumed, std::move(left.value()), std::move(right.value()));
}

[[nodiscard]]
static Node parseAssignment(Parser& parser, Node prev, Token consumed, std::uint32_t prec) {
    // std::cout << "in correct method" << std::endl;
    auto right = parser.parseExpression();
    if (!right) {
        throw std::runtime_error("Failed to parse right side of assignment");
    }
    return std::make_unique<AssignmentExpr>(std::move(prev), std::move(right.value()));
}

[[nodiscard]]
static Node parseScope(Parser& parser, Token consumed) {
    // std::cout << "Tryna parse scope " << std::endl;
    auto ret = std::make_unique<Scope>(parser.parseStatements());
    parser.consumeTokenOfType(TokenType::Right_Brace);
    return ret;
}

[[nodiscard]]
static Node parseFunctionCall(Parser& parser, Node prev, Token consumed, std::uint32_t _prec) {
    auto args = parseParenGroup(parser, std::move(consumed));
    return std::make_unique<FunctionCall>(std::move(prev), args ? std::optional{std::move(args)} : std::nullopt);
}

[[nodiscard]]
static Node parseIf(Parser& parser, Token consumed) {
    parser.consumeTokenOfType(TokenType::Left_Parenthesis);
    auto cond = parser.parseExpression();
    if (!cond) {
        throw std::runtime_error("Failed to parse condition of if");
    }
    parser.consumeTokenOfType(TokenType::Right_Parenthesis);
    auto thenBranch = parser.parseStatement();
    if (!thenBranch) {
        throw std::runtime_error("Failed to parse then branch of if");
    }

    auto elseBranch = [&parser]() -> std::optional<Node> {
        if (parser.isNextTokenOfType(TokenType::Else)) {
            parser.consumeTokenOfType(TokenType::Else);
            return std::optional(parser.parseStatement());
        }
        return std::nullopt;
    }();

    return std::make_unique<If>(std::move(cond.value()), std::move(thenBranch.value()), std::move(elseBranch));
}

[[nodiscard]]
static Node parseWhile(Parser& parser, Token consumed) {
    parser.consumeTokenOfType(TokenType::Left_Parenthesis);
    auto cond = parser.parseExpression();
    if (!cond) {
        throw std::runtime_error("Failed to parse while condition");
    }
    parser.consumeTokenOfType(TokenType::Right_Parenthesis);
    auto body = parser.parseStatement();
    if (!body) {
        throw std::runtime_error("Failed to parse while body");
    }
    return std::make_unique<While>(std::move(cond.value()), std::move(body.value()));
}

[[nodiscard]]
static Node parseArrayIndexing(Parser& parser, Node prev, Token _consumed, std::uint32_t prec) {
    auto index = parser.parseExpression(); // not using prec cause otherwise nothing will be found
    if (!index) {
        throw std::runtime_error("No index supplied for array indexing");
    }
    parser.consumeTokenOfType(TokenType::Right_Bracket);
    return std::make_unique<ArrayIndexing>(std::move(prev), std::move(index.value()));
}

[[nodiscard]]
static Node parseReturn(Parser& parser, Token consumed) {
    if (auto next = parser.lookaheadToken(0); next && next->type == TokenType::Semicolon) {
        parser.consumeTokenOfType(TokenType::Semicolon);
        return std::make_unique<Return>();
    }
    auto what = parser.parseExpression();
    if (!what) {
        throw std::runtime_error("Failed to parse return expression");
    }
    parser.consumeTokenOfType(TokenType::Semicolon);
    return std::make_unique<Return>(std::move(what.value()));
}

[[nodiscard]]
std::optional<Node> Parser::parseExprWithPrecedence(std::uint32_t prec) {
    auto maybeToken = consumeNextToken();
    if (!maybeToken) {
        return std::nullopt;
    }
    Token token = *maybeToken;

    auto prefixParser = subParsers[token.type].prefix;

    if (prefixParser == nullptr)
        throw std::runtime_error("Error parsing token " + token.toString() + ". Expected an expression");

    auto parsedPrefix = prefixParser(*this, token);
    // std::cout << std::format("parsed prefix: {}\n", parsedPrefix->sExpression());

    while (prec <= getPrecedenceOfNext()) {
        if (parsedPrefix->isStatement())
            return parsedPrefix;
        maybeToken = consumeNextToken();
        if (!maybeToken) {
            return parsedPrefix;
        }
        token = *maybeToken;
        auto infixParser = subParsers[token.type].infix;
        auto newPrec = subParsers[token.type].infixPrecedence +
                       (subParsers[token.type].associativity == Associativity::Right ? 0 : 1);
        parsedPrefix = infixParser(*this, std::move(parsedPrefix), token, newPrec);
    }
    return parsedPrefix;
}

[[nodiscard]]
std::optional<Node> Parser::parseExpression() {
    return parseExprWithPrecedence(Precedence::PREC_ASSIGNMENT);
};

[[nodiscard]]
std::vector<Node> Parser::parseStatements() {
    std::vector<Node> statements;
    for (auto statement = parseStatement(); statement; statement = parseStatement()) {
        statements.push_back(std::move(*statement));
    }
    return statements;
}

[[nodiscard]]
std::optional<Node> Parser::parseStatement() {
    if (auto lookahead = lookaheadToken(0); lookahead && lookahead->type == TokenType::Right_Brace) {
        return std::nullopt; // end of scope
    }
    auto expr = parseExprWithPrecedence(Precedence::PREC_ASSIGNMENT);
    if (!expr)
        return std::nullopt;
    if (expr.value()->isStatement())
        return expr;
    consumeTokenOfType(TokenType::Semicolon);
    return std::make_unique<ExpressionStatement>(std::move(expr.value()));
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
        // std::cout << functions.back()->toString() << std::endl;
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
    subParsers[TokenType::Number] = {&parseValue, nullptr, Precedence::PREC_NONE, Associativity::None};
    subParsers[TokenType::Identifier] = {&parseIdentifier, nullptr, Precedence::PREC_NONE, Associativity::None};

    subParsers[TokenType::Minus] = {&parsePrefixOperator, &parseBinaryOperator, Precedence::PREC_PLUS_MINUS,
                                    Associativity::Left};
    subParsers[TokenType::Plus] = {nullptr, &parseBinaryOperator, Precedence::PREC_PLUS_MINUS, Associativity::Left};
    subParsers[TokenType::Star] = {nullptr, &parseBinaryOperator, Precedence::PREC_MUL_DIV_MOD, Associativity::Left};
    subParsers[TokenType::Slash] = {nullptr, &parseBinaryOperator, Precedence::PREC_MUL_DIV_MOD, Associativity::Left};
    subParsers[TokenType::Mod] = {nullptr, &parseBinaryOperator, Precedence::PREC_MUL_DIV_MOD, Associativity::Left};

    subParsers[TokenType::Exclamation_Mark] = {&parsePrefixOperator, nullptr, Precedence::PREC_NONE,
                                               Associativity::None};
    subParsers[TokenType::Sizespec] = {nullptr, &parseBinaryOperator, PREC_ASSIGNMENT, Associativity::None};

    subParsers[TokenType::Tilde] = {&parsePrefixOperator, nullptr, Precedence::PREC_NONE, Associativity::None};
    subParsers[TokenType::And_Bit] = {&parsePrefixOperator, &parseBinaryOperator, Precedence::PREC_LOGIC_BIT_AND,
                                      Associativity::Left};
    subParsers[TokenType::Or_Bit] = {nullptr, &parseBinaryOperator, Precedence::PREC_LOGIC_BIT_OR, Associativity::Left};
    subParsers[TokenType::Xor] = {nullptr, &parseBinaryOperator, PREC_LOGIC_BIT_XOR, Associativity::Left};
    subParsers[TokenType::Bitshift_Left] = {nullptr, &parseBinaryOperator, PREC_BIT_SHIFT, Associativity::Left};
    subParsers[TokenType::Bitshift_Right] = {nullptr, &parseBinaryOperator, PREC_BIT_SHIFT, Associativity::Left};

    subParsers[TokenType::And_Logical] = {nullptr, &parseBinaryOperator, Precedence::PREC_LOGIC_AND,
                                          Associativity::Left};
    subParsers[TokenType::Or_Logical] = {nullptr, &parseBinaryOperator, Precedence::PREC_LOGIC_OR, Associativity::Left};

    subParsers[TokenType::Left_Parenthesis] = {&parseParenGroup, &parseFunctionCall, Precedence::PREC_ARRAY,
                                               Associativity::Left};
    subParsers[TokenType::Right_Parenthesis] = {nullptr, nullptr, Precedence::PREC_NONE, Associativity::None};
    subParsers[TokenType::Left_Brace] = {&parseScope, nullptr, PREC_NONE, Associativity::None};
    subParsers[TokenType::Right_Brace] = {nullptr, nullptr, PREC_NONE, Associativity::None};
    subParsers[TokenType::Left_Bracket] = {nullptr, &parseArrayIndexing, PREC_ARRAY, Associativity::Left};
    subParsers[TokenType::Right_Bracket] = {nullptr, nullptr, PREC_NONE, Associativity::None};

    subParsers[TokenType::Semicolon] = {&parseEmptyStatement, nullptr, Precedence::PREC_NONE, Associativity::None};
    subParsers[TokenType::Assignment] = {nullptr, &parseAssignment, PREC_ASSIGNMENT, Associativity::Right};
    // Comparisons
    subParsers[TokenType::Equals] = {nullptr, &parseBinaryOperator, PREC_IS_EQUALS, Associativity::Left};
    subParsers[TokenType::Uneqal] = {nullptr, &parseBinaryOperator, PREC_IS_EQUALS, Associativity::Left};
    subParsers[TokenType::Larger] = {nullptr, &parseBinaryOperator, PREC_ARITH_COMP, Associativity::Left};
    subParsers[TokenType::Larger_Equal] = {nullptr, &parseBinaryOperator, PREC_ARITH_COMP, Associativity::Left};
    subParsers[TokenType::Smaller] = {nullptr, &parseBinaryOperator, PREC_ARITH_COMP, Associativity::Left};
    subParsers[TokenType::Smaller_Equal] = {nullptr, &parseBinaryOperator, PREC_ARITH_COMP, Associativity::Left};

    // Keywords
    subParsers[TokenType::If] = {&parseIf, nullptr, PREC_NONE, Associativity::None};
    subParsers[TokenType::Else] = {nullptr, nullptr, PREC_NONE, Associativity::None};
    subParsers[TokenType::Register] = {&parseAssignment, nullptr, PREC_NONE, Associativity::None};
    subParsers[TokenType::Auto] = {&parseAssignment, nullptr, PREC_NONE, Associativity::None};
    subParsers[TokenType::While] = {&parseWhile, nullptr, PREC_NONE, Associativity::None};
    subParsers[TokenType::Return] = {&parseReturn, nullptr, PREC_NONE, Associativity::None};
    subParsers[TokenType::Comma] = {nullptr, &parseCommaList, PREC_ASSIGNMENT, Associativity::Right};
}

::AST::AST Parser::parse() {
    registerAllSubParsers();
    auto funcs = parseFunctions();
    if (tokens.size() != 0) {
        throw std::runtime_error("Malformed program");
    }
    return ::AST::AST{std::move(funcs)};
}

} // namespace ParsingInternals

AST::AST parse(std::span<const Token> tokens) {
    ParsingInternals::Parser p{tokens};
    auto ast = p.parse();
    ast.analyze();
    return ast;
}
