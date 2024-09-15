#include "CFG.h"
#include "Parser.h"
#include "Scanner.h"
#include <gtest/gtest.h>

using namespace CFG;
auto program =
    R"(    

        bFunc() {
        }

        callTest(a, b) {
            register c = bFunc();
            return bFunc() + bFunc() + bFunc();
        }  

        ifTest(x) {
            if (x < -5)
                return;
            x = x + 3;
        } 

        gauss(x) {
            register res = -0;
            while (x > 0) {
                res = res + x;
                x = x - 1;
            }
            return res;
        }
        )";

TEST(CFGTests, CFSContainsRightFunctions) {
    auto lexed{scan(program)};
    auto ast{parse(lexed)};
    auto cfg{generateCFG(ast)};
    ASSERT_EQ(cfg.functions.size(), 4);
    ASSERT_TRUE(cfg.functions.contains("gauss"));
    ASSERT_TRUE(cfg.functions.contains("ifTest"));
    ASSERT_TRUE(cfg.functions.contains("callTest"));
    ASSERT_TRUE(cfg.functions.contains("bFunc"));
}

TEST(CFGTests, EmptyFunctionGeneratesOnlyProAndEpilogue) {
    auto lexed{scan(program)};
    auto ast{parse(lexed)};
    auto cfg{generateCFG(ast)};
    auto& bFunc = cfg.functions.at("bFunc");
    ASSERT_EQ(bFunc.type, BlockType::FunctionPrologue);
    ASSERT_EQ(bFunc.posterior.size(), 1);
    ASSERT_EQ(bFunc.extraInfo.size(), 1);
    ASSERT_EQ(bFunc.extraInfo.at(0), nullptr);

    auto& epilogue = *bFunc.posterior.at(0);
    ASSERT_EQ(epilogue.type, BlockType::FunctionEpilogue);
    ASSERT_EQ(epilogue.posterior.size(), 0);
    ASSERT_EQ(epilogue.extraInfo.size(), 0);
}

TEST(CFGTests, IfWithoutElseWorks) {
    auto lexed{scan(program)};
    auto ast{parse(lexed)};
    auto cfg{generateCFG(ast)};
    auto& ifTest = cfg.functions.at("ifTest");
    ASSERT_EQ(ifTest.type, BlockType::FunctionPrologue);
    auto& ifBlcok = *ifTest.posterior.at(0);
    ASSERT_EQ(ifBlcok.type, BlockType::If);
    ASSERT_EQ(ifBlcok.posterior.size(), 3);
    ASSERT_EQ(ifBlcok.posterior[1], nullptr); // the else
    auto& thenB = *ifBlcok.posterior.at(0);
    auto& postIfB = *ifBlcok.posterior.at(2);
    ASSERT_EQ(thenB.type, BlockType::Return);
    ASSERT_EQ(postIfB.type, BlockType::Normal);
}

TEST(CFGTests, whileWorks) {
    auto lexed{scan(program)};
    auto ast{parse(lexed)};
    auto cfg{generateCFG(ast)};
    auto& whileTest = cfg.functions.at("gauss");
    ASSERT_EQ(whileTest.type, BlockType::FunctionPrologue);
    auto& whileBlcok = *whileTest.posterior.at(0)->posterior.at(0);
    ASSERT_EQ(whileBlcok.type, BlockType::While);
    ASSERT_EQ(whileBlcok.posterior.size(), 2);
    ASSERT_EQ(whileBlcok.posterior.at(0)->posterior.at(0).get(), &whileBlcok);
}
