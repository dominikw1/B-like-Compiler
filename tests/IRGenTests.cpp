#include "CFG.h"
#include "Parser.h"
#include "SSAGeneration.h"
#include "Scanner.h"
#include "llvm/IR/Verifier.h"
#include <gtest/gtest.h>

#define VERIFY_VALID(program)                                                                                          \
    do {                                                                                                               \
        auto ast = parse(scan(program));                                                                               \
        auto cfg = CFG::generateCFG(ast);                                                                              \
        auto ir = generateIR(cfg);                                                                                     \
        ir.module->dump();                                                                                             \
        bool isWrong = llvm::verifyModule(*ir.module, &llvm::outs());                                                  \
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
