#include "CFG.h"
#include "Parser.h"
#include "SSAGeneration.h"
#include "Scanner.h"
#include <gtest/gtest.h>

TEST(IRGenTests, generatesSimpleExprStatement) {
    auto program = "main() {return 3;}";
    auto ast = parse(scan(program));
    auto cfg = CFG::generateCFG(ast);
    auto ir = generateIR(cfg);
}