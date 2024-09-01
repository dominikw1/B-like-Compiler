#pragma once
#include "../Parser/AST.h";
#include <functional>
#include <memory>
#include <unordered_map>
#include <vector>

enum class BlockType { FunctionPrologue, FunctionEpilogue, FunctionCall, While, If, Normal, Return };

struct BasicBlock {
    BlockType type;
    std::vector<const Expression*> extraInfo;
    std::vector<std::shared_ptr<BasicBlock>> posterior;
    BasicBlock(BlockType t, std::vector<std::shared_ptr<BasicBlock>> post) : type{t}, posterior{std::move(post)} {}
    BasicBlock(BlockType t, std::vector<std::shared_ptr<BasicBlock>> post, std::vector<const Expression*> extraInfo)
        : type{t}, posterior{std::move(post)}, extraInfo{std::move(extraInfo)} {}

    void replaceByIf(std::shared_ptr<BasicBlock>, std::function<bool(const BasicBlock&)> pred);
    void print(const BasicBlock* until = nullptr);
};

struct CFG {
    std::unordered_map<std::string_view, BasicBlock> functions;
};

// Lifetime of AST must be at least as long as that of CFG
CFG generateCFG(const AST& ast);