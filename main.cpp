#include "Parser.h"
#include "Scanner.h"
#include <iostream>
#include <span>

int main() {
    auto lexed{scan("1+3\n;\nakfak\nhihi\n")};
    Parser parser{lexed};
    AST ast{parser.parse()};

    // for (const auto l : lexed) {
    //     std::cout << tokenTypeToString(l.type) << " " << l.lexeme << "\n";
    // }
}