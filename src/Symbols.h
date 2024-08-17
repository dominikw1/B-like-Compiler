#pragma once
#include <cstdint>
#include <iostream>
#include <string_view>
#include <tuple>
#include <unordered_map>

enum class VariableType { Auto, Register, Parameter };

struct VariableSymbol {
    std::uint32_t depthDecl;
    VariableType type;
};

struct FunctionSymbol {
    std::uint32_t numArgs;
};

struct SymbolScope {
    std::unordered_map<std::string, FunctionSymbol> functions;
    std::unordered_map<std::string, VariableSymbol> variables;

    void dump() const {
        for (auto& function : functions) {
            std::cout << "Function " << function.first << " with " << function.second.numArgs << " args" << std::endl;
        }
        for (auto& var : variables) {
            std::cout << "Variable " << var.first << " at depth " << var.second.depthDecl << " of type ";
            switch (var.second.type) {
            case VariableType::Auto:
                std::cout << "auto";
                break;
            case VariableType::Register:
                std::cout << "register";
                break;
            case VariableType::Parameter:
                std::cout << "Parameter";
                break;
            }
            std::cout << std::endl;
        }
    }
};