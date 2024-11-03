#pragma once
#include <cstdint>
#include <iostream>
#include <memory>
#include <optional>
#include <string_view>
#include <tuple>
#include <unordered_map>
#include <unordered_set>

template <typename... Bases> struct overload : Bases... {
    using is_transparent = void;
    using Bases::operator()...;
};

struct char_pointer_hash {
    auto operator()(const char* ptr) const noexcept { return std::hash<std::string_view>{}(ptr); }
};

using transparent_string_hash = overload<std::hash<std::string>, std::hash<std::string_view>, char_pointer_hash>;

enum class VariableType { Auto, Register, Parameter };

struct VariableSymbol {
    std::uint32_t depthDecl;
    VariableType type;
};

struct FunctionSymbol {
    std::uint32_t numArgs;
};

struct SymbolScope {
    std::unordered_map<std::string_view, FunctionSymbol, transparent_string_hash, std::equal_to<>> functions;
    std::unordered_map<std::string_view, VariableSymbol, transparent_string_hash, std::equal_to<>> variables;
    SymbolScope* parent = nullptr;
    std::unordered_set<std::string_view, transparent_string_hash, std::equal_to<>> undecidedParams;

    std::optional<FunctionSymbol> getFunctionAtCurr(std::string_view name) const {
        if (auto foundIt = functions.find(name); foundIt != functions.end()) {
            return foundIt->second;
        }
        return std::nullopt;
    }

    template <typename SymbolType> bool raiseParamAtCurr(std::string_view name, SymbolType symbol) {
        if (undecidedParams.contains(name)) {
            undecidedParams.erase(name);
            if constexpr (std::is_same<SymbolType, FunctionSymbol>::value) {
                functions[name] = symbol;
            } else {
                variables[name] = symbol;
            }
            return true;
        } else {
            if (parent)
                return parent->raiseParamAtCurr(name, symbol);
            return false; // throw std::runtime_error(std::format("Parameter {} does not exist!", name));
        }
    }

    bool isUndecidedParameter(std::string_view name) const {
        return undecidedParams.contains(name) || (parent && parent->isUndecidedParameter(name));
    }

    std::optional<VariableSymbol> getVariableAtCurr(std::string_view name) const {
        if (auto foundIt = variables.find(name); foundIt != variables.end()) {
            return foundIt->second;
        }

        return std::nullopt;
    }

    std::optional<VariableSymbol> getOrTransformVariable(std::string_view name) {
        if (auto var = getVariable(name)) {
            return var;
        }
        if (raiseParamAtCurr(name, VariableSymbol{0, VariableType::Parameter})) {
            return getVariable(name);
        }
        return std::nullopt;
    }

    std::optional<FunctionSymbol> getOrTransformFunction(std::string_view name, std::uint32_t numArgs) {
        if (auto func = getFunction(name)) {
            return func;
        }
        if (raiseParamAtCurr(name, FunctionSymbol{numArgs}))
            return getFunction(name);
        return std::nullopt;
    }

    bool symbolExists(std::string_view name) const {
        if (getFunctionAtCurr(name) || getVariableAtCurr(name) || undecidedParams.contains(name))
            return true;
        if (parent) {
            return parent->symbolExists(name);
        }
        return false;
    }

    std::optional<VariableSymbol> getVariable(std::string_view name) const {
        if (auto var = getVariableAtCurr(name); var) {
            return var;
        }
        return (parent ? parent->getVariable(name) : std::nullopt);
    }

    std::optional<FunctionSymbol> getFunction(std::string_view name) const {
        if (auto func = getFunctionAtCurr(name); func) {
            return func;
        }
        return (parent ? parent->getFunction(name) : std::nullopt);
    }

    std::unique_ptr<SymbolScope> duplicate() {
        auto scope = std::make_unique<SymbolScope>();
        scope->parent = this;
        return scope;
    };

    void addFunctionToToplevel(std::string_view name, FunctionSymbol symb) {
        if (parent) {
            parent->addFunctionToToplevel(name, std::move(symb));
            return;
        }
        if (functions.contains(name)) {
            throw std::runtime_error("function already in module");
        }
        functions[name] = symb;
    }

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