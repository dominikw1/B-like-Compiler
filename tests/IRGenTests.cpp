#include "CFG.h"
#include "Parser.h"
#include "SSAGeneration.h"
#include "Scanner.h"
#include "llvm/IR/Verifier.h"
#include <gtest/gtest.h>

TEST(IRGenTests, generatesSimpleReturnStatement) {
    auto program = "main() {return 3;}";
    auto ast = parse(scan(program));
    auto cfg = CFG::generateCFG(ast);
    auto ir = generateIR(cfg);
    ir.module->dump();
    ASSERT_TRUE(llvm::verifyModule(*ir.module));
}

TEST(IRGenTests, generatesSimpleBoolExpr) {
    auto program = "main(a,b) {a && b;}";
    auto ast = parse(scan(program));
    auto cfg = CFG::generateCFG(ast);
    auto ir = generateIR(cfg);
    ir.module->dump();
    ASSERT_TRUE(llvm::verifyModule(*ir.module));
}