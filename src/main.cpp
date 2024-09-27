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
    mul(a,b){
    return a*b;
    }

  main() {return mul(5,3);}
    )";

#include "llvm/Bitcode/BitcodeWriter.h"
#include <fstream>

int main(int argc, char* argv[]) {
    std::string program = testProgram;
    if (argc > 1) {
        std::ifstream input{std::string(argv[1])};
        program = std::string((std::istreambuf_iterator<char>(input)), std::istreambuf_iterator<char>());
    }

    auto lexed{scan(program)};
    auto ast{parse(lexed)};
    auto cfg{CFG::generateCFG(ast)};
    auto ir{generateIR(cfg)};

    std::error_code EC;
    llvm::raw_fd_ostream OS("module", EC);
    WriteBitcodeToFile(*ir.module, OS);
    OS.flush();

    return 0;
}