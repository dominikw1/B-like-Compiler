#include <algorithm>
#include <cctype>
#include <iostream>
#include <optional>
#include <ranges>
#include <string>
#include <string_view>
#include <vector>

#include "Token.h"

constexpr auto isNumber(char c) {
    // maybe look at hex / octa
    return c >= '0' && c <= '9';
};

bool isNextChar(auto curr, auto end, char c) {
    if (curr + 1 == end) {
        return false;
    }
    return *(curr + 1) == c;
}

constexpr std::string_view parseNumber(auto& curr, auto end) {
    auto numStart = curr;
    while (curr != end) {
        if (!isNumber(*curr))
            break;
        ++curr;
    }
    return std::string_view{numStart, curr};
}

constexpr static std::optional<std::string_view> tryParseExactMatch(auto& curr, auto end, std::string_view match) {
    auto helper = curr;
    for (auto& c : match) {
        if (isNextChar(helper, end, c)) {
            ++helper;
        } else {
            return {};
        }
    }
    ++helper;
    return std::string_view{curr, helper};
}

constexpr static std::optional<Token> tryParseKeyword(auto& curr, auto end) {
    if (auto m = tryParseExactMatch(curr, end, "while"))
        return Token{*m, TokenType::While};
    if (auto m = tryParseExactMatch(curr, end, "if"))
        return Token{*m, TokenType::If};
    if (auto m = tryParseExactMatch(curr, end, "else"))
        return Token{*m, TokenType::Else};
    if (auto m = tryParseExactMatch(curr, end, "auto"))
        return Token{*m, TokenType::Auto};
    if (auto m = tryParseExactMatch(curr, end, "register"))
        return Token{*m, TokenType::Register};
    if (auto m = tryParseExactMatch(curr, end, "return"))
        return Token{*m, TokenType::Return};
    return {};
}

constexpr static std::optional<Token> tryParseIdentifier(auto& curr, auto end) {
    auto it = curr;
    if (!std::isalpha(*it) && *it != '_')
        return {};
    ++it;
    while (it != end && (std::isalnum(*it) || *it == '_')) {
        ++it;
    }
    Token ret{std::string_view{curr, it}, TokenType::Identifier};
    curr = it;
    return ret;
}

// precondition: curr - curr+n is valid view
std::string_view incrementCurr(auto& currIt, std::size_t len) {
    auto ret = std::string_view{currIt, len};
    currIt += len;
    return ret;
}

std::optional<Token> lexNextToken(auto& curr, auto end) {
    if (curr == end)
        return {};
    if (isNumber(*curr)) {
        return Token{parseNumber(curr, end), TokenType::Number};
    }

    switch (*curr) {
    case ' ':
    case '\t':
    case '\n':
        return lexNextToken(++curr, end);
    case '+':
        return Token{incrementCurr(curr, 1), TokenType::Plus};
    case '-':
        return Token{incrementCurr(curr, 1), TokenType::Minus};
    case ',':
        return Token{incrementCurr(curr, 1), TokenType::Comma};
        //  case '.':
        //    return Token{incrementCurr(curr, 1), TokenType::Dot};
    case '/':
        return Token{incrementCurr(curr, 1), TokenType::Slash};
    case '*':
        return Token{incrementCurr(curr, 1), TokenType::Star};
    case '!':
        if (isNextChar(curr, end, '='))
            return Token{incrementCurr(curr, 2), TokenType::Uneqal};
        return Token{incrementCurr(curr, 1), TokenType::Exclamation_Mark};
    case '=':
        return Token{incrementCurr(curr, 1), TokenType::Assignment};
    case '>':
        if (isNextChar(curr, end, '='))
            return Token{incrementCurr(curr, 2), TokenType::Larger_Equal};
        if (isNextChar(curr, end, '>'))
            return Token{incrementCurr(curr, 2), TokenType::Bitshift_Right};
        return Token{incrementCurr(curr, 1), TokenType::Larger};
    case '<':
        if (isNextChar(curr, end, '='))
            return Token{incrementCurr(curr, 2), TokenType::Smaller_Equal};
        if (isNextChar(curr, end, '>'))
            return Token{incrementCurr(curr, 2), TokenType::Bitshift_Left};
        return Token{incrementCurr(curr, 1), TokenType::Smaller};
    case '^':
        return Token{incrementCurr(curr, 1), TokenType::Xor};
    case '&':
        if (isNextChar(curr, end, '&'))
            return Token{incrementCurr(curr, 2), TokenType::And_Logical};
        return Token{incrementCurr(curr, 1), TokenType::And_Bit};
    case '|':
        if (isNextChar(curr, end, '|'))
            return Token{incrementCurr(curr, 2), TokenType::Or_Logical};
        return Token{incrementCurr(curr, 1), TokenType::Or_Bit};
    case '~':
        return Token{incrementCurr(curr, 1), TokenType::Tilde};
    case ';':
        return Token{incrementCurr(curr, 1), TokenType::Semicolon};
    case '(':
        return Token{incrementCurr(curr, 1), TokenType::Left_Parenthesis};
    case ')':
        return Token{incrementCurr(curr, 1), TokenType::Right_Parenthesis};
    case '[':
        return Token{incrementCurr(curr, 1), TokenType::Left_Bracket};
    case ']':
        return Token{incrementCurr(curr, 1), TokenType::Right_Bracket};
    case '{':
        return Token{incrementCurr(curr, 1), TokenType::Left_Brace};
    case '}':
        return Token{incrementCurr(curr, 1), TokenType::Right_Brace};
    case '%':
        return Token{incrementCurr(curr, 1), TokenType::Mod};
    case '@':
        return Token{incrementCurr(curr, 1), TokenType::Sizespec};
    }
    if (auto k = tryParseKeyword(curr, end)) {
        return *k;
    }
    if (auto i = tryParseIdentifier(curr, end)) {
        return *i;
    }

    return Token{"", TokenType::ERROR};
}

auto scan(std::string_view program) {
    auto curr = program.begin();
    std::vector<Token> tokens;
    while (curr != program.end()) {
        if (auto newToken = lexNextToken(curr, program.end())) {
            tokens.push_back(*newToken);
        } else {
            break;
        }
    }
    return tokens;
}