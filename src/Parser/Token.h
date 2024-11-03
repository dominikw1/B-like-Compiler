#pragma once
#include <stdexcept>
#include <string>
#include <string_view>

enum class TokenType {
    Semicolon,
    Plus,
    Minus,
    Left_Parenthesis,
    Right_Parenthesis,
    Left_Bracket,
    Right_Bracket,
    Left_Brace,
    Right_Brace,
    Comma,
    Slash,
    Star,
    Exclamation_Mark,
    Uneqal,
    Assignment,
    Equals,
    Larger,
    Larger_Equal,
    Smaller,
    Smaller_Equal,
    Identifier,
    Number,
    And_Logical,
    And_Bit,
    Or_Logical,
    Or_Bit,
    If,
    Else,
    While,
    Xor,
    Bitshift_Left,
    Bitshift_Right,
    Mod,
    Tilde,
    Auto,
    Register,
    Return,
    Sizespec,
};

inline std::string_view tokenTypeToSymbol(const TokenType type) {
    switch (type) {
    case TokenType::Plus:
        return "+";
    case TokenType::Star:
        return "*";
    case TokenType::Slash:
        return "/";
    case TokenType::Minus:
        return "-";
    case TokenType::Semicolon:
        return ";";
    case TokenType::Left_Parenthesis:
        return "(";
    case TokenType::Right_Parenthesis:
        return ")";
    case TokenType::Left_Brace:
        return "[";
    case TokenType::Right_Brace:
        return "]";
    case TokenType::Assignment:
        return "=";
    case TokenType::And_Bit:
        return "&";
    case TokenType::And_Logical:
        return "&&";
    case TokenType::Bitshift_Left:
        return "<<";
    case TokenType::Bitshift_Right:
        return ">>";
    case TokenType::Comma:
        return ",";
    case TokenType::Equals:
        return "==";
    case TokenType::Exclamation_Mark:
        return "!";
    case TokenType::Larger:
        return ">";
    case TokenType::Larger_Equal:
        return ">=";
    case TokenType::Smaller:
        return "<";
    case TokenType::Smaller_Equal:
        return "<=";
    case TokenType::Mod:
        return "%";
    case TokenType::Or_Bit:
        return "|";
    case TokenType::Or_Logical:
        return "||";
    case TokenType::Sizespec:
        return "@";
    case TokenType::Tilde:
        return "~";
    case TokenType::Uneqal:
        return "!=";
    case TokenType::Xor:
        return "^";
    default:
        throw std::runtime_error("unimplemented");
    };
}

inline std::string_view tokenTypeToString(const TokenType type) {
    switch (type) {
    case TokenType::Plus:
        return "Plus";
    case TokenType::Star:
        return "Star";
    case TokenType::Slash:
        return "Slash";
    case TokenType::Minus:
        return "Minus";
    case TokenType::Semicolon:
        return "Semicolon";
    case TokenType::Left_Parenthesis:
        return "Left_parenthesis";
    case TokenType::Right_Parenthesis:
        return "Right_parenthesis";
    case TokenType::Left_Brace:
        return "Left_Brace";
    case TokenType::Right_Brace:
        return "Right_Brace";
    case TokenType::Identifier:
        return "identifier";
    case TokenType::Number:
        return "Number";
    case TokenType::Assignment:
        return "Assignment";
    default:
        return "unimplemented";
    };
}

struct Token {
    TokenType type;
    std::string_view lexeme;
    // line num?
    Token(std::string_view lexeme, TokenType type) : type{type}, lexeme{lexeme} {}

    [[nodiscard]]
    std::string toString() {
        return std::string(lexeme) + " of type " + std::string(tokenTypeToString(type));
    }

    bool operator==(const Token& other) const { return type == other.type && lexeme == other.lexeme; }
};