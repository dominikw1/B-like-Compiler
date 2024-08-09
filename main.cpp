#include "Parser.h"
#include "Scanner.h"
#include <iostream>
int main() {
    auto lexed{scan(";\nakfak\nhihi\n")};
    Parser parser{lexed};
    AST ast{parser.parse()};

    // for (const auto l : lexed) {
    //     std::cout << tokenTypeToString(l.type) << " " << l.lexeme << "\n";
    // }
}