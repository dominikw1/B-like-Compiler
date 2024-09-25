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

        ifElseTest(x) {
            if (x < -5)
                return;
            else
                x = x+1;
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

BasicBlock getFunction(auto& map, std::string nameWanted) {
    auto it = std::find_if(map.begin(), map.end(), [&nameWanted](auto& p) {
        auto& [name, cf] = p;
        return name == nameWanted;
    });
    if (it != map.end()) {
        auto& [name, cf] = *it;
        return cf;
    }
    throw std::runtime_error("no function of this type");
}

TEST(CFGTests, CFSContainsRightFunctions) {
    auto lexed{scan(program)};
    auto ast{parse(lexed)};
    auto cfg{generateCFG(ast)};
    ASSERT_EQ(cfg.functions.size(), 5);
    ASSERT_NO_THROW(getFunction(cfg.functions, "gauss"));
    ASSERT_NO_THROW(getFunction(cfg.functions, "ifTest"));
    ASSERT_NO_THROW(getFunction(cfg.functions, "ifElseTest"));
    ASSERT_NO_THROW(getFunction(cfg.functions, "callTest"));
    ASSERT_NO_THROW(getFunction(cfg.functions, "bFunc"));
    ASSERT_THROW(getFunction(cfg.functions, "skfkfskaksaffksa"), std::runtime_error);
}

TEST(CFGTests, EmptyFunctionGeneratesOnlyProAndEpilogue) {
    auto lexed{scan(program)};
    auto ast{parse(lexed)};
    auto cfg{generateCFG(ast)};
    auto bFunc = getFunction(cfg.functions, "bFunc");
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
    auto ifTest = getFunction(cfg.functions, "ifTest");
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

TEST(CFGTests, ifWithElseWorks) {
    auto lexed{scan(program)};
    auto ast{parse(lexed)};
    auto cfg{generateCFG(ast)};
    auto ifTest = getFunction(cfg.functions, "ifElseTest");
    ASSERT_EQ(ifTest.type, BlockType::FunctionPrologue);
    auto& ifBlcok = *ifTest.posterior.at(0);
    ASSERT_EQ(ifBlcok.type, BlockType::If);
    ASSERT_EQ(ifBlcok.posterior.size(), 3);
    ASSERT_NE(ifBlcok.posterior[1], nullptr);
    auto& thenB = *ifBlcok.posterior.at(0);
    auto& elseB = *ifBlcok.posterior.at(1);
    auto& postIfB = *ifBlcok.posterior.at(2);
    ASSERT_EQ(thenB.type, BlockType::Return);
    ASSERT_EQ(elseB.type, BlockType::Normal);
    ASSERT_EQ(postIfB.type, BlockType::Normal);
}

TEST(CFGTests, whileWorks) {
    auto lexed{scan(program)};
    auto ast{parse(lexed)};
    auto cfg{generateCFG(ast)};
    auto whileTest = getFunction(cfg.functions, "gauss");
    ASSERT_EQ(whileTest.type, BlockType::FunctionPrologue);
    auto& whileBlcok = *whileTest.posterior.at(0)->posterior.at(0);
    ASSERT_EQ(whileBlcok.type, BlockType::While);
    ASSERT_EQ(whileBlcok.posterior.size(), 2);
    //  ASSERT_EQ(whileBlcok.posterior.at(0)->posterior.at(0).get(), &whileBlcok);
}
