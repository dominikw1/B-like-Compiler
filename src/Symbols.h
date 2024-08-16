#pragma once
#include <cstdint>
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
    std::unordered_map<std::string_view, FunctionSymbol> functions;
    std::unordered_map<std::string_view, VariableSymbol> variables;
};