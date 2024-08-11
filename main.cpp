#include "Parser.h"
#include "Scanner.h"
#include <iostream>
#include <span>

int main() {
    try {
        auto lexed{scan("1+3\n;5*2+2+5/4;\nakfak;\nhihi;\nfoo+bar;FO+bar;;")};
        for (auto& l : lexed) {
            std::cout << l.lexeme << " " << std::endl;
        }
        Parser parser{lexed};

        AST ast{parser.parse()};

    } catch (std::exception& e) {
        std::cout << "exception " << e.what() << std::endl;
    }
}