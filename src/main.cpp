#include "IRGenerator/SSAGeneration.h"
#include "Parser/Parser.h"
#include "Parser/Scanner.h"
#include <iostream>
#include <span>

constexpr auto websiteProgram = R"(  
    phis(a, b){
        a = a * b;
        if (a > b * b) {
            register c = 1;
            while (a > 0)
                a = a - c;
        } else {
            a = b * b;
        }
        return a;
    }

    deadcode(a, b, c) {
        if (a)
            return a;
        else
            return b;
        return c;
    }

    fnA() {}
    fnB() {}
    fnC() {}

    shortcircuit() {
        return fnA() && fnB() || fnC();
    }

    undef() { return; }
)";

constexpr auto testProgram =
    R"(
  main(a) {auto a = 1; while(a){a = 0;} return 2;}
    )";

#include "llvm/Bitcode/BitcodeWriter.h"

int main() {
    auto lexed{scan(testProgram)};
    auto ast{parse(lexed)};
    auto cfg{CFG::generateCFG(ast)};
    auto ir{generateIR(cfg)};

    std::error_code EC;
    llvm::raw_fd_ostream OS("module", EC);
    WriteBitcodeToFile(*ir.module, OS);
    OS.flush();

    return 0;
}