#include "../src/Scanner.h"
#include <gtest/gtest.h>

TEST(LexerTests, LexerRecognisesKeywordIf) {
    auto scanned = scan("if");
    Token correct{"if", TokenType::If};
    ASSERT_TRUE(correct == scanned.at(0));
}
TEST(LexerTests, LexerRecognisesEquals) {
    auto scanned = scan("==");
    Token correct{"==", TokenType::Equals};
    ASSERT_TRUE(correct == scanned.at(0));
}

TEST(LexerTests, DoesNotGetConfusedWithKeywordPrefixIdentifier) {
    auto scanned = scan("ifNotKeyword");
    Token correct{"ifNotKeyword", TokenType::Identifier};
    std::cout<<tokenTypeToString(scanned.at(0).type);
    ASSERT_EQ(correct, scanned.at(0));
}

TEST(LexerTests, LexerRecognisesElseKW) {
    auto scanned = scan("else");
    Token correct{"else", TokenType::Else};
    ASSERT_TRUE(correct == scanned.at(0));
}

