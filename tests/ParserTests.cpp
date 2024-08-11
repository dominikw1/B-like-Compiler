#include "../src/Parser.h"
#include "../src/Scanner.h"
#include <gtest/gtest.h>

auto parseProgram(std::string_view program) {
    auto lexed = scan(program);
    Parser p{lexed};
    return p.parse();
}

TEST(ParserTests, ParserParsesIfWithoutElseCorrectly) {
    auto program = "if(a==b){a = 5;}";
    auto ast = parseProgram(program);
    ASSERT_EQ(ast.getTopLevel().at(0)->getType(), ExpressionType::If);
}