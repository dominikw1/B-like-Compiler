#pragma once
#include "../Parser/AST.h";
#include <memory>
#include <unordered_map>
#include <vector>

enum class BlockType { FunctionPrologue, FunctionEpilogue, FunctionCall, While, If, Normal };

struct BasicBlock {
    BlockType type;
    std::vector<const Expression*> extraInfo;
    std::vector<std::unique_ptr<BasicBlock>> posterior;
    BasicBlock(BlockType t, std::vector<std::unique_ptr<BasicBlock>> post) : type{t}, posterior{std::move(post)} {}
    BasicBlock(BlockType t, std::vector<std::unique_ptr<BasicBlock>> post, std::vector<const Expression*> extraInfo)
        : type{t}, posterior{std::move(post)}, extraInfo{std::move(extraInfo)} {}
};

class CFG {
    std::unordered_map<std::string_view, BasicBlock> functions;
};

CFG generateCFG(AST ast);