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

TEST(IRGenTests, generatesSimpleBoolExpr) { VERIFY_VALID("main(a,b) {a && b;}"); }