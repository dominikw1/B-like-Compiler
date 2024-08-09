#include <algorithm>
#include <cctype>
#include <iostream>
#include <ranges>
#include <string>
#include <string_view>
#include <vector>

#include "Token.h"

constexpr TokenType getTokenTypeForOneCharLiteral(std::string_view lexeme) {
    auto oneChar = lexeme.front();
    switch (oneChar) {
    case '+':
        return TokenType::Plus;
    case '-':
        return TokenType::Minus;
    case ',':
        return TokenType::Comma;
    case '.':
        return TokenType::Dot;
    case '/':
        return TokenType::Slash;
    case '*':
        return TokenType::Star;
    case '!':
        return TokenType::Exclamation_Mark;
    case '=':
        return TokenType::Assignment;
    case '>':
        return TokenType::Larger;
    case '<':
        return TokenType::Smaller;
    case '^':
        return TokenType::Xor;
    case '&':
        return TokenType::And_Bit;
    case '|':
        return TokenType::Or_Bit;
    case '~':
        return TokenType::Tilde;
    case ';':
        return TokenType::Semicolon;
    case '(':
        return TokenType::Left_Parenthesis;
    case ')':
        return TokenType::Right_Parenthesis;
    case '[':
        return TokenType::Left_Bracket;
    case ']':
        return TokenType::Right_Bracket;
    case '{':
        return TokenType::Left_Brace;
    case '}':
        return TokenType::Right_Brace;
    case '%':
        return TokenType::Mod;
    default:
        return TokenType::Undecided;
    }
}

constexpr TokenType getTokenTypeForTwoCharLiteral(std::string_view lexeme) {
    auto firstChar = lexeme.front();
    switch (firstChar) {
    case '!':
        return lexeme.at(2) == '=' ? TokenType::Uneqal : TokenType::ERROR;
    case '>':
        return lexeme.at(2) == '=' ? TokenType::Larger_Equal
                                   : (lexeme.at(2) == '>' ? TokenType::Bitshift_Right : TokenType::ERROR);
    case '<':
        return lexeme.at(2) == '=' ? TokenType::Smaller_Equal
                                   : (lexeme.at(2) == '>' ? TokenType::Bitshift_Left : TokenType::ERROR);
    case '&':
        return lexeme.at(2) == '&' ? TokenType::And_Logical : TokenType::ERROR;
    case '|':
        return lexeme.at(2) == '|' ? TokenType::Or_Logical : TokenType::ERROR;
    default:
        return TokenType::Undecided;
    };
}

constexpr TokenType getTokenTypeForComplexLiteral(std::string_view lexeme) {
    constexpr auto isNumber = [](std::string_view lex) {
        return lex.size() != 0 && std::all_of(lex.begin(), lex.end(), [](char c) { return c >= '0' && c <= '9'; });
    };
    if (isNumber(lexeme))
        return TokenType::Number;
    if (lexeme.at(0) == '@' && isNumber(lexeme.substr(1)))
        return TokenType::Sizespec;
    if (lexeme == "while")
        return TokenType::While;
    if (lexeme == "if")
        return TokenType::If;
    if (lexeme == "else")
        return TokenType::Else;
    if (lexeme == "auto")
        return TokenType::Auto;
    if (lexeme == "register")
        return TokenType::Register;
    if (lexeme == "return")
        return TokenType::Return;

    const char firstChar = lexeme.front();
    if (std::isalpha(firstChar) || firstChar == '_') {
        if (std::all_of(lexeme.begin(), lexeme.end(), [](char c) { return std::isalnum(c) || c == '_'; }))
            return TokenType::Identifier;
        else
            return TokenType::ERROR;
    }
    return TokenType::ERROR;
}

constexpr TokenType getTokenType(std::string_view lexeme) {
    if (lexeme.size() == 1) {
        const auto type = getTokenTypeForOneCharLiteral(lexeme);
        if (type != TokenType::Undecided)
            return type;
    }
    if (lexeme.size() == 2) {
        const auto type = getTokenTypeForTwoCharLiteral(lexeme);
        if (type != TokenType::Undecided)
            return type;
    }

    return getTokenTypeForComplexLiteral(lexeme);
}

constexpr Token constructToken(std::string_view lexeme) {
    if (lexeme.empty())
        return Token{TokenType::ERROR, "Empty lexeme"};
    TokenType type = getTokenType(lexeme);
    return Token{type, lexeme};
}

auto scan(std::string_view program) {
    using std::operator""sv;
    constexpr auto lineDelim{"\n"sv};
    constexpr auto wordDelim(" "sv);
    auto tokens = std::views::split(program, lineDelim) |
                  std::views::transform([wordDelim](auto&& line) { return std::views::split(line, wordDelim); }) |
                  std::views::join | std::views::filter([](auto&& word) { return !word.empty(); }) |
                  std::views::transform([&](auto&& lexeme) {
                      return Token{getTokenType(std::string_view(lexeme)), std::string_view(lexeme)};
                  });
    std::vector<Token> mergedTokens;
    for (auto t : tokens) {
        mergedTokens.push_back(t);
    }
    return mergedTokens;
}