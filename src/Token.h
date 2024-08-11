#pragma once
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
    // Dot,
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
    ERROR,
    Undecided,
};

inline std::string_view tokenTypeToString(const TokenType type) {
    switch (type) {
    case TokenType::ERROR:
        return "ERROR";
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
    case TokenType::Left_Brace:
        return "Left_Brace";
    case TokenType::Right_Brace:
        return "Right_Brace";
    case TokenType::Identifier:
        return "identifier";
    case TokenType::Number:
        return "Number";
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