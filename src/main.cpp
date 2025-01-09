#include "AssemblyPrinter/ASMPrinter.h"
#include "IRGenerator/SSAGeneration.h"
#include "InstructionSelector/InstructionSelector.h"
#include "Optimizer/Optimizer.h"
#include "Parser/AST.h"
#include "Parser/Parser.h"
#include "Parser/Scanner.h"
#include "RegisterAllocator/RegisterAllocator.h"
#include <fstream>
#include <sstream>
#include <stdexcept>
#include <string>

int main(int argc, char** argv) {
    if (argc != 3) {
        std::cerr << "Usage: " << argv[0] << " {-c/-a/-l/-i/-r} <input>";
        return EXIT_FAILURE;
    }
    try {
        std::ifstream input{std::string(argv[2])};
        std::string program{std::istreambuf_iterator<char>(input), std::istreambuf_iterator<char>()};
        auto lexed{scan(program)};
        auto ast{parse(lexed)};

        if (std::string_view{argv[1]}.starts_with("-c")) {
            // only AST build and sema
            return EXIT_SUCCESS;
        }

        if (std::string_view{argv[1]}.starts_with("-a")) {
            std::cout << ast.sExpression() << "\n";
            return EXIT_SUCCESS;
        }

        auto IR = generateIR(std::move(ast));
        optimize(IR.module.get());

        if (std::string_view{argv[1]}.starts_with("-l")) {
            IR.module->print(llvm::outs(), nullptr);
            return EXIT_SUCCESS;
        }

        auto normalFunctions = doInstructionSelection(*IR.module);
        if (std::string_view{argv[1]}.starts_with("-i")) {
            IR.module->print(llvm::outs(), nullptr);
            return EXIT_SUCCESS;
        }

        allocateRegisters(*IR.module, normalFunctions);
        if (std::string_view{argv[1]}.starts_with("-r")) {
            IR.module->print(llvm::outs(), nullptr);
            return EXIT_SUCCESS;
        }

        auto assembly = turnToASM(*IR.module);
        llvm::outs() << assembly << "\n";
        return EXIT_SUCCESS;
    } catch (std::exception& e) {
        std::cerr << e.what() << "\n";
        return EXIT_FAILURE;
    }
}