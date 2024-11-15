#include "Parser.h"
#include "SSAGeneration.h"
#include "Scanner.h"
#include "llvm/IR/Verifier.h"
#include <gtest/gtest.h>

#define VERIFY_VALID(program)                                                                                          \
    do {                                                                                                               \
        auto ast = parse(scan(program));                                                                               \
        ast.analyze();                                                                                                 \
        auto IR = generateIR(std::move(ast));                                                                              \
        IR.module->dump();                                                                                             \
        bool isWrong = llvm::verifyModule(*IR.module, &llvm::outs());                                                  \
        ASSERT_FALSE(isWrong);                                                                                         \
    } while (0);

TEST(IRGenTests, emptyVoidMain) { VERIFY_VALID("main() {}"); }
TEST(IRGenTests, returnConstant) { VERIFY_VALID("foo() {return 3;}"); }
TEST(IRGenTests, addParameterWithConst) { VERIFY_VALID("bar(a){return a+5;}"); }
TEST(IRGenTests, simpleBoolExpr) { VERIFY_VALID("main(a,b) {a && b;}"); }
TEST(IRGenTests, simpleMinus) { VERIFY_VALID("main(a) {return a -1;}"); }
TEST(IRGenTests, simpleMul) { VERIFY_VALID("main(a) {return a*2;}"); }
TEST(IRGenTests, simpleDiv) { VERIFY_VALID("main(a) {return a/2;}"); }
TEST(IRGenTests, complexArith) { VERIFY_VALID("main(a) {return 2+1*5515/(2*5)-2*111*a/2;}"); }
TEST(IRGenTests, negation) { VERIFY_VALID("main(a) {return -a;}"); }
TEST(IRGenTests, lotsOfParentheses) { VERIFY_VALID("main(a) {return -((((a)))+2);}"); }
TEST(IRGenTests, autoLocalVar) { VERIFY_VALID("main(a) {auto i = 1; return a+i;}"); }
TEST(IRGenTests, simpleIfElse) { VERIFY_VALID("main(a) {if(a) {return a;} else {return 0;}}"); }
TEST(IRGenTests, simpleIf) { VERIFY_VALID("main(a) {if(a) {return a;} return 4;}"); }
TEST(IRGenTests, simpleIfUsingAutoVar) { VERIFY_VALID("main(a) {auto b = 2; if(b) {return a;} return 4;}"); }
TEST(IRGenTests, simpleIfUsingParameter) { VERIFY_VALID("main(a) {auto b = 2; if(b) {return b;} return 4;}"); }
TEST(IRGenTests, simpleIfPartialReturn) {
    VERIFY_VALID("main(a) {auto b = 2; if(b) {return b;} else {b = 4;} return b;}");
}
TEST(IRGenTests, simpleIfPartialReturnInElse) {
    VERIFY_VALID("main(a) {auto b = 2; if(b) {b=5;} else {return 4+b;} return b;}");
}
TEST(IRGenTests, nestedIf) { VERIFY_VALID("main(a) {auto b = 2; if(b) {if(a){return b;}} return 2;}"); }
TEST(IRGenTests, nestedIfWithElse) {
    VERIFY_VALID("main(a) {auto b = 2; if(b) {if(a){return b;}else {b=4;}} return 2;}");
}
TEST(IRGenTests, veryNestedIf) {
    VERIFY_VALID("main(a) {auto b = 2; if(b) "
                 "{if(a){if(a+1)if(a+2)if(a*5)if(a+b)if(b)if(10000*a)if(1)if(a*100+2)if(b)return b;}else {b=4;}} "
                 "return 2;}");
}

TEST(IRGenTests, variableDeclInIf) {
    VERIFY_VALID(
        R"(
    main(a) {
        if(a){
            auto c = 1;
        }
    })");
}

TEST(IRGenTests, variableDeclInIfWithUsage) {
    VERIFY_VALID(
        R"(
    main(a) {
        if(a){
            auto c = 1;
            if(a+1) {
                if(c) {
                    return c;
                }  
                return a;
            }
        }
        return 0;
    })");
}

TEST(IRGenTests, ifWithComplexCondition) {
    VERIFY_VALID(
        R"(
    main(a, b) { 
        if(a&&b){
           return b;
        }
        return 0;
    })");
}

TEST(IRGenTests, ifVeryComplexCondition) {
    VERIFY_VALID(
        R"(
    main(a, b) {
        auto c = 2; 
        if(a&&b&&c&&(a+c)&&(b*c+ a) && (a&&0)){
           return b;
        }
        return 0;
    })");
}

TEST(IRGenTests, ifElseComplexConditionsWithVarAllocation) {
    VERIFY_VALID(
        R"(
    main(a, b) {
        auto c = 2; 
        if(a&&b&&c&&(a+c)&&(b*c+ a) && (a&&0)){
           auto d = 2;
           return d;
        } else {
            auto d = 5;
            if(d&&a&&(d*a)) {
                auto e=4;
                d=6;
                e=1;
                if(e) {
                return 0;
                }
            }
        }
        return 0;
    })");
}

TEST(IRGenTests, trivialWhile){VERIFY_VALID("main(b) {auto a = 1; while(a){return 0;} return 2;}")}

TEST(IRGenTests, trivialWhileChangingCondition){VERIFY_VALID("main(b) {auto a = 1; while(a){a = 0;} return 2;}")}

TEST(IRGenTests, complexWhileConditionImmutable) {
    VERIFY_VALID("main(b) {auto a = 1; while((a+2)&&(a-1)&&(a)&&(a&&a)){auto c= 2;} return 2;}");
}

TEST(IRGenTests, complexWhileConditionMutable) {
    VERIFY_VALID("main(b) {auto a = 1; auto d = 4; while((a+2)&&(a-1)&&(a)&&(d+a)&&(a*d)&&(a&&a)){auto c= 2; d = c; a "
                 "= d*2;} return 2;}");
}

TEST(IRGenTests, whileAndIfCombined) {
    VERIFY_VALID(
        R"(
            main(a,b) {
                auto c = 1;
                while(c && b) {
                    if(b+1) {
                        c = c + a;
                    }
                }
            }
    )");
}

TEST(IRGenTests, mutableParam) {
    VERIFY_VALID(
        R"(
            main(a,b) {
                b = 2*a;
                return b;
            }
    )");
}

TEST(IRGenTests, registerVar) {
    VERIFY_VALID(
        R"(
            main() {
                register c = 1;
                return c*5;
            }
    )");
}

TEST(IRGenTests, moreOperations) {
    VERIFY_VALID(
        R"(
            main(a) {
                auto c = 1;
                c = 1 << 6;
                c = c >> 2;
                c = (c|a) & 9999;
                c = 1 > 3;
                c = c ^ (c+2);
                return c != 5;
            }
    )");
}

TEST(IRGenTests, trivialFunctionCall) {
    VERIFY_VALID(
        R"(
            foo() {
            }
            main() {
               foo();
            }
    )");
}

TEST(IRGenTests, functionCallWithParam) {
    VERIFY_VALID(
        R"(
            foo(a) {
            return a;
            }
            main(a) {
               foo(a);
            }
    )");
}