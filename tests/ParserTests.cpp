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
    auto& ifExpr{NODE_AS_REF(ast.getTopLevel().at(0), If)};
    ASSERT_EQ(ifExpr.condition->getType(), ExpressionType::BinaryOperator);
    auto& equalsExpr{NODE_AS_REF(ifExpr.condition, BinaryOperator)};
    auto& aInCond = NODE_AS_REF(equalsExpr.operand1, Name);
    auto& bInCond = NODE_AS_REF(equalsExpr.operand2, Name);
    ASSERT_EQ(aInCond.literal, "a");
    ASSERT_EQ(bInCond.literal, "b");
    ASSERT_TRUE(NODE_IS(ifExpr.thenBranch, Scope));
    auto& scope = NODE_AS_REF(ifExpr.thenBranch, Scope);
    ASSERT_TRUE(NODE_IS(scope.scoped, ExpressionStatement));
    auto& assignmentStatement = NODE_AS_REF(scope.scoped, ExpressionStatement);
    auto assignemntExpr = CAST_NODE_IF_TYPE(assignmentStatement.expression, Assignment);
    ASSERT_TRUE(assignemntExpr);
    auto assignmentA = CAST_NODE_IF_TYPE(assignemntExpr->left, Name);
    auto assignment5 = CAST_NODE_IF_TYPE(assignemntExpr->right, Value);
    ASSERT_TRUE(assignmentA && assignmentA->literal == "a");
    ASSERT_TRUE(assignment5 && assignment5->val == 5);
}
TEST(ParserTests, ParserParsesIfWithElseCorrectly) {
    auto program = "if(a==b){a = 5;}else a = 6;";
    auto ast = parseProgram(program);
    auto ifExpr = CAST_NODE_IF_TYPE(ast.getTopLevel().at(0), If);
    ASSERT_TRUE(ifExpr && ifExpr->condition && ifExpr->thenBranch && ifExpr->elseBranch);
    auto elseExpr = CAST_NODE_IF_TYPE(ifExpr->elseBranch, Scope);
    ASSERT_TRUE(elseExpr && elseExpr->scoped);
    auto assignmentStatement = CAST_NODE_IF_TYPE(elseExpr->scoped, ExpressionStatement);
    ASSERT_TRUE(assignmentStatement && assignmentStatement->expression);
    auto assignment = CAST_NODE_IF_TYPE(assignmentStatement->expression, Assignment);
    ASSERT_TRUE(assignment);
    auto a = CAST_NODE_IF_TYPE(assignment->left, Name);
    ASSERT_TRUE(a && a->literal == "a");

    auto expr6 = CAST_NODE_IF_TYPE(assignment->right, Value);
    ASSERT_TRUE(expr6 && expr6->val == 6);
}