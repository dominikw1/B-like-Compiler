#include <Parser.h>
#include <Scanner.h>
#include <algorithm>
#include <functional>
#include <gtest/gtest.h>
#include <iostream>
#include <iterator>
#include <numeric>
#include <ranges>
#include <vector>

using namespace std::literals;
using namespace AST;

#define WRAPPED_IN_MAIN(program) ("main(){" program "}")
#define ASSERT_AND_CONVERT(node, type)                                                                                 \
    [&]() -> const type* {                                                                                             \
        assert(NODE_IS(node, type));                                                                                   \
        return NODE_AS_PTR(node, type);                                                                                \
    }();

AST::AST parseProgram(std::string_view program) {
    auto lexed = scan(program);
    ParsingInternals::Parser p{lexed};
    auto AST = p.parse();
    std::cout << AST.sExpression() << std::endl;
    return AST;
}

TEST(ParserTests, ParserParsesIfWithoutElseCorrectly) {
    auto program = WRAPPED_IN_MAIN("if(a==b)a = 5;");
    auto ast = parseProgram(program);
    auto& statements = NODE_AS_REF(ast.getTopLevel().at(0), Function).body;
    ASSERT_EQ(statements.at(0)->getType(), ExpressionType::If);
    auto& ifExpr{NODE_AS_REF(statements.at(0), If)};
    ASSERT_EQ(ifExpr.condition->getType(), ExpressionType::BinaryOperator);
    auto& equalsExpr{NODE_AS_REF(ifExpr.condition, BinaryOperator)};
    auto& aInCond = NODE_AS_REF(equalsExpr.operand1, Name);
    auto& bInCond = NODE_AS_REF(equalsExpr.operand2, Name);
    ASSERT_EQ(aInCond.literal, "a");
    ASSERT_EQ(bInCond.literal, "b");
    ASSERT_EQ(ifExpr.thenBranch->getType(), ExpressionType::ExpressionStatement);

    auto& assignmentStatement =
        NODE_AS_REF(NODE_AS_REF(ifExpr.thenBranch, ExpressionStatement).expression, AssignmentExpr);
    auto assignmentA = CAST_NODE_IF_TYPE(assignmentStatement.left, Name);
    auto assignment5 = CAST_NODE_IF_TYPE(assignmentStatement.right, Value);
    ASSERT_TRUE(assignmentA && assignmentA->literal == "a");
    ASSERT_TRUE(assignment5 && assignment5->val == 5);
}
TEST(ParserTests, ParserParsesIfWithElseCorrectly) {
    auto program = WRAPPED_IN_MAIN("if(a==b)a = 5; else a = 6;");
    auto ast = parseProgram(program);
    auto& statements = NODE_AS_REF(ast.getTopLevel().at(0), Function).body;
    auto ifExpr = CAST_NODE_IF_TYPE(statements.at(0), If);
    ASSERT_TRUE(ifExpr && ifExpr->condition && ifExpr->thenBranch && ifExpr->elseBranch);
    std::cout << ifExpr->elseBranch.value()->toString() << std::endl;

    auto assignment =
        CAST_NODE_IF_TYPE(NODE_AS_REF(ifExpr->elseBranch.value(), ExpressionStatement).expression, AssignmentExpr);
    ASSERT_TRUE(assignment);
    auto a = CAST_NODE_IF_TYPE(assignment->left, Name);
    ASSERT_TRUE(a && a->literal == "a");

    auto expr6 = CAST_NODE_IF_TYPE(assignment->right, Value);
    ASSERT_TRUE(expr6 && expr6->val == 6);
}

TEST(ParserTests, ParserParsesCorrectNumberOfTopLineFunctions) {
    auto program = ("main() {} other() {} lol() {}");
    auto ast = parseProgram(program);
    ASSERT_EQ(ast.getTopLevel().size(), 3);
}

TEST(ParserTests, ParserParsesCorrectFunctionNames) {
    auto program = ("main() {} other() {} lol() {}");
    auto ast = parseProgram(program);
    auto* main = NODE_AS_PTR(ast.getTopLevel().at(0), Function);
    auto& mainName = NODE_AS_REF(main->name, Name);
    ASSERT_EQ(mainName.literal, "main");

    auto* other = NODE_AS_PTR(ast.getTopLevel().at(1), Function);
    auto& otherName = NODE_AS_REF(other->name, Name);
    ASSERT_EQ(otherName.literal, "other");

    auto* lol = NODE_AS_PTR(ast.getTopLevel().at(2), Function);
    auto& lolName = NODE_AS_REF(lol->name, Name);
    ASSERT_EQ(lolName.literal, "lol");
}

TEST(ParserTests, ParserParsesAssignmentWithModyfierCorrectly) {
    auto program = WRAPPED_IN_MAIN("register a = 2; auto a = 2;");
    auto ast = parseProgram(program);
    auto& main = NODE_AS_REF(ast.getTopLevel()[0], Function);
    ASSERT_TRUE(NODE_IS(main.body.at(0), Assignment));
    auto& statement1 = NODE_AS_REF(main.body.at(0), Assignment);
    ASSERT_TRUE(statement1.modifyer);
    ASSERT_EQ(statement1.modifyer->type, TokenType::Register);
    ASSERT_TRUE(NODE_IS(statement1.left, Name));
    ASSERT_TRUE(NODE_IS(statement1.right, Value));
    ASSERT_TRUE(NODE_IS(main.body.at(1), Assignment));
    auto& statement2 = NODE_AS_REF(main.body.at(1), Assignment);
    ASSERT_TRUE(statement2.modifyer);
    ASSERT_EQ(statement2.modifyer->type, TokenType::Auto);
    ASSERT_TRUE(NODE_IS(statement2.left, Name));
    ASSERT_TRUE(NODE_IS(statement2.right, Value));
}

TEST(ParserTests, ParserParsesWhileCorrectly) {
    auto program = WRAPPED_IN_MAIN("while(1) {doStuff;}");
    auto ast = parseProgram(program);
    auto& main = NODE_AS_REF(ast.getTopLevel().at(0), Function);
    ASSERT_TRUE(NODE_IS(main.body.at(0), While));
    auto& whileSt = NODE_AS_REF(main.body.at(0), While);
    ASSERT_TRUE(NODE_IS(whileSt.condition, Value));
    ASSERT_TRUE(NODE_IS(whileSt.body, Scope));
    auto& body = NODE_AS_REF(whileSt.body, Scope);
    ASSERT_TRUE(NODE_IS(body.scoped.at(0), ExpressionStatement));
}

TEST(ParserTests, ParserParsesEmptyReturnCorrectly) {
    auto program = WRAPPED_IN_MAIN("return;");
    auto ast = parseProgram(program);
    auto& main = NODE_AS_REF(ast.getTopLevel().at(0), Function);
    std::cout << main.body.at(0)->toString() << std::endl;
    ASSERT_TRUE(NODE_IS(main.body.at(0), Return));
    auto& returnSt = NODE_AS_REF(main.body.at(0), Return);
    ASSERT_FALSE(returnSt.what);
}

TEST(ParserTests, ParserFailsWithDuplicateFunctionParamNames) {
    auto program = "fn(a,b,a){}";
    auto ast = parseProgram(program);
    ASSERT_ANY_THROW(ast.analyze());
}

TEST(ParserTests, ParserParsesNonEmptyReturnCorrectly) {
    auto program = WRAPPED_IN_MAIN("return 5*1+2;");
    auto ast = parseProgram(program);
    auto& main = NODE_AS_REF(ast.getTopLevel().at(0), Function);
    ASSERT_TRUE(NODE_IS(main.body.at(0), Return));
    auto& returnSt = NODE_AS_REF(main.body.at(0), Return);
    ASSERT_TRUE(returnSt.what);
    ASSERT_TRUE(NODE_IS(returnSt.what.value(), BinaryOperator));
    auto& retVal = NODE_AS_REF(returnSt.what.value(), BinaryOperator);
    ASSERT_EQ(retVal.type, TokenType::Plus);
}

TEST(ParserTests, ParserFunctionCall) {
    auto program = WRAPPED_IN_MAIN("fun();fun(a,b,c);fun()+fun(a,b);");
    auto ast = parseProgram(program);
    auto& main = NODE_AS_REF(ast.getTopLevel().at(0), Function);
    ASSERT_TRUE(
        std::all_of(main.body.cbegin(), main.body.cend(), [](auto& n) { return NODE_IS(n, ExpressionStatement); }));
    auto& st1 = NODE_AS_REF(main.body.at(0), ExpressionStatement);
    auto& st2 = NODE_AS_REF(main.body.at(1), ExpressionStatement);
    auto& st3 = NODE_AS_REF(main.body.at(2), ExpressionStatement);
    ASSERT_TRUE(NODE_IS(st1.expression, FunctionCall));
    ASSERT_TRUE(NODE_IS(st2.expression, FunctionCall));
    ASSERT_EQ(st3.expression->getType(), ExpressionType::BinaryOperator);

    auto& call1 = NODE_AS_REF(st1.expression, FunctionCall);
    auto& call2 = NODE_AS_REF(st2.expression, FunctionCall);
    auto& add = NODE_AS_REF(st3.expression, BinaryOperator);

    auto& call1Name = NODE_AS_REF(call1.name, Name);
    auto& call2Name = NODE_AS_REF(call2.name, Name);
    ASSERT_EQ(call1Name.literal, "fun");
    ASSERT_EQ(call2Name.literal, "fun");

    ASSERT_FALSE(call1.args);
    ASSERT_TRUE(call2.args);

    auto& argListWithParens = *ASSERT_AND_CONVERT(call2.args.value(), Parenthesised);
    auto& argList = NODE_AS_REF(argListWithParens.inner.value(), CommaList);
    auto& a = *ASSERT_AND_CONVERT(argList.left, Name);
    auto& bSide = *ASSERT_AND_CONVERT(argList.right, CommaList);
    auto& b = *ASSERT_AND_CONVERT(bSide.left, Name);
    auto& c = *ASSERT_AND_CONVERT(bSide.right, Name);
    ASSERT_EQ(a.literal, "a");
    ASSERT_EQ(b.literal, "b");
    ASSERT_EQ(c.literal, "c");

    auto& f1 = *ASSERT_AND_CONVERT(add.operand1, FunctionCall);
    auto& f2 = *ASSERT_AND_CONVERT(add.operand2, FunctionCall);

    auto& f1callName = NODE_AS_REF(f1.name, Name);
    auto& f2callName = NODE_AS_REF(f2.name, Name);
    ASSERT_EQ(f1callName.literal, "fun");
    ASSERT_EQ(f2callName.literal, "fun");

    ASSERT_FALSE(f1.args);
    ASSERT_TRUE(f2.args);
    ASSERT_TRUE(NODE_IS(f2.args.value(), Parenthesised));
    auto& f2argListPs = NODE_AS_REF(f2.args.value(), Parenthesised);
    auto& f2argList = *ASSERT_AND_CONVERT(f2argListPs.inner.value(), CommaList);
    auto& f2a = *ASSERT_AND_CONVERT(f2argList.left, Name);
    auto& f2b = *ASSERT_AND_CONVERT(f2argList.right, Name);
    ASSERT_EQ(f2a.literal, "a");
    ASSERT_EQ(f2b.literal, "b");
}

TEST(ParserTests, ArrayIndexingNormal) {
    auto program = WRAPPED_IN_MAIN("a[1];b[a*23];");
    auto ast = parseProgram(program);
    auto& main = *ASSERT_AND_CONVERT(ast.getTopLevel().at(0), Function);
    auto& st1 = *ASSERT_AND_CONVERT(main.body.at(0), ExpressionStatement);
    auto& st2 = *ASSERT_AND_CONVERT(main.body.at(1), ExpressionStatement);

    auto& indexing1 = *ASSERT_AND_CONVERT(st1.expression, ArrayIndexing);
    auto& indexing2 = *ASSERT_AND_CONVERT(st2.expression, ArrayIndexing);

    auto& arr1 = *ASSERT_AND_CONVERT(indexing1.array, Name);
    ASSERT_EQ(arr1.literal, "a");
    auto& arr2 = *ASSERT_AND_CONVERT(indexing2.array, Name);
    ASSERT_EQ(arr2.literal, "b");
    auto& index1 = *ASSERT_AND_CONVERT(indexing1.index, Value);
    ASSERT_EQ(index1.val, 1);
    auto& index2 = *ASSERT_AND_CONVERT(indexing2.index, BinaryOperator);
    auto& index2A = *ASSERT_AND_CONVERT(index2.operand1, Name);
    auto& index2_23 = *ASSERT_AND_CONVERT(index2.operand2, Value);

    ASSERT_EQ(index2A.literal, "a");
    ASSERT_EQ(index2_23.val, 23);
    ASSERT_EQ(index2.type, TokenType::Star);
}

TEST(ParserTests, ArraySizespec) {
    auto program = WRAPPED_IN_MAIN("a[b@8];");
    auto ast = parseProgram(program);
    auto& main = *ASSERT_AND_CONVERT(ast.getTopLevel().at(0), Function);
    auto& st1 = *ASSERT_AND_CONVERT(main.body.at(0), ExpressionStatement);
    auto& indexing1 = *ASSERT_AND_CONVERT(st1.expression, ArrayIndexing);
    auto& sizespec = *ASSERT_AND_CONVERT(indexing1.index, BinaryOperator);
    ASSERT_EQ(sizespec.type, TokenType::Sizespec);
    auto& b = *ASSERT_AND_CONVERT(sizespec.operand1, Name);
    auto& sizeOp = *ASSERT_AND_CONVERT(sizespec.operand2, Value);
}

auto websiteProgram = R"(  
    gauss(x) {
        register res = -0;
        while (x > 0) {
            res = res + x;
            x = x - 1;
        }
        return res;
    }

    ifTest(x) {
        if (x < -5)
            return;
        x = x + 3;
    }

    isBool(x) { return !!x == x; }

    callTest(a, b) {
        register c = foo(a, b);
        return bar(c, a) + baf(a) + baz(c);
    }

    baz(a) { return; }

    unreachableCode(a) {
        if (a > 0) return a;
        else return -a;
        return a + 1;
    }

    foo(a, b) {
        a[b] = b;
        return a[b] + a[b@1];
    }

    addrof(ptr) {
        auto var = 1;
        ptr[1] = &var;
        register ptr2 = &ptr[1];
        ptr2[0] = 2;
        return var;
    }
)";

TEST(ParserTests, ParserDoesNotThrowWithExampleFromWebsite) { ASSERT_NO_THROW(parseProgram(websiteProgram)); }

void parseAndAnalyseProgram(std::string_view program) {
    auto lexed = scan(program);
    ParsingInternals::Parser p{lexed};
    auto AST{p.parse()};
    AST.analyze();
}

static auto err = [](std::string_view program) {
    std::cerr << std::format("{}", program) << std::endl;
    ASSERT_ANY_THROW(parseAndAnalyseProgram(program));
};
static auto ok = [](std::string_view program) {
    std::cerr << std::format("{}", program) << std::endl;
    ASSERT_NO_THROW(parseAndAnalyseProgram(program));
};

TEST(ParserTests, ParserDoesNotThrowWithVariousExamples) {
    ok("f(a, b) { g(a)[0] = b; }");
    err("f(){//}");
    err("fn(x) { f(1); f(2, 3); }");
    ok("f(fn) { fn(fn); }");
}

TEST(ParserTests, callNonNameFunctionFails) { err("fn(x) { fn(x)(x); }"); }

TEST(ParserTests, calls) {
    ok("f(fn) { fn(fn); }");
    ok("f(x) { return x; } g(x) { return f(x); }");
    ok("g(x) { return f(x + 1); } f(x) { return x; }");
    ok("g(x) { return f(x + 1); }");
    err("fn(x) { (fn)(x); }");
    err("fn(x) { (x)(x); }");
    err("fn(x) { f(1); f(2, 3); }");
    err("fn(x) { f(1, 3); f(2); }");
    err("f(a) {} fn(x) { f(1, 3); }");
    err("f(a, b) {} fn(x) { f(1); }");
    err("f() {} fn(x) { f(1); }");
    err("fn(x) { f(1, 3); } f(a) {}");
    err("fn(x) { f(1); } f(a, b) {}");
    err("fn(x) { f(1); } f(){}");
}

TEST(ParserTests, decls) {
    ok("fn(a){register b = a;}");
    ok("fn(a){auto b = a;}");
    ok("fn(a){{register a = a;}}");
    ok("fn(a){{auto a = a;}}");
    err("fn(a){{auto a = &a;}}");
    err("fn(){if(1)register a = 1;}");
    err("fn(){if(1){}else register a = 1;}");
    ok("fn(){if(1){}else{register a = 1;}}");
}

TEST(ParserTests, subscripts) {
    ok("f(a, b) { return a[b]; }");
    ok("f(a, b) { return a[b@1]; }");
    ok("f(a, b) { return a[b@2]; }");
    err("f(a, b) { return a[b@3]; }");
    ok("f(a, b) { return a[b@4]; }");
    err("f(a, b) { return a[b@5]; }");
    err("f(a, b) { return a[b@6]; }");
    err("f(a, b) { return a[b@7]; }");
    ok("f(a, b) { return a[b@8]; }");
    err("f(a, b) { return a[b@9]; }");
    err("f(a, b) { return a[b@10]; }");
    err("f(a, b) { return a[b@a]; }");
    err("f(a, b) { return a[b@]; }");
}

TEST(ParserTests, addrof) {
    ok("f() { auto a = 0; return &a; }");
    ok("f(a, b) { return &a[b]; }");
    ok("f(a, b) { return &a[b@1]; }");
    ok("f(a, b) { return &a[b@2]; }");
    ok("f(a, b) { return &a[b@4]; }");
    ok("f(a, b) { return &a[b@8]; }");
    err("f(a) { return &a; }");
    err("f() { register a = 0; return &a; }");
}