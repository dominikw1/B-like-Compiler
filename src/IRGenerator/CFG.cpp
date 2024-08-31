#include "CFG.h"
#include <optional>
#include <ranges>
#include <span>

std::unique_ptr<BasicBlock> generateFromStatement(std::vector<const Expression*>::iterator statementsBegin,
                                                  std::vector<const Expression*>::iterator statementsEnd);

template <typename T> std::vector<const T*> rawPointerize(const std::vector<std::unique_ptr<T>>& vec) {
    std::vector<const T*> ptrs(vec.size());
    std::transform(vec.begin(), vec.end(), ptrs.begin(), [](const auto& ptr) { return ptr.get(); });
    return ptrs;
}

std::unique_ptr<BasicBlock> generateForWhile(const Expression& curr, std::unique_ptr<BasicBlock> posterior) {
    const While& whileSt = static_cast<const While&>(curr);

    std::unique_ptr<BasicBlock> subBlock = nullptr;
    if (NODE_IS(whileSt.body, Scope)) {
        auto& scope = NODE_AS_REF(whileSt.body, Scope);
        auto scoped{rawPointerize(scope.scoped)};
        subBlock = generateFromStatement(scoped.begin(), scoped.end());

    } else {
        std::vector<const Expression*> temp{whileSt.body.get()};
        subBlock = generateFromStatement(temp.begin(), temp.end());
    }

    std::vector<std::unique_ptr<BasicBlock>> post;
    post.push_back(std::move(subBlock));
    post.push_back(std::move(posterior));
    return std::make_unique<BasicBlock>(BlockType::While, std::move(post),
                                        std::vector<const Expression*>{whileSt.condition.get()});
}

std::unique_ptr<BasicBlock> generateForIf(const Expression& curr, std::unique_ptr<BasicBlock> posterior) {
    const If& ifSt = static_cast<const If&>(curr);
    std::unique_ptr<BasicBlock> thenBlock = nullptr;
    if (NODE_IS(ifSt.thenBranch, Scope)) {
        auto& scope = NODE_AS_REF(ifSt.thenBranch, Scope);
        auto ptrs = rawPointerize(scope.scoped);
        thenBlock = generateFromStatement(ptrs.begin(), ptrs.end());
    } else {
        auto temp = std::vector<const Expression*>{ifSt.thenBranch.get()};
        thenBlock = generateFromStatement(temp.begin(), temp.end());
    }

    std::unique_ptr<BasicBlock> elseBlock = nullptr;
    if (auto& elseBranch = ifSt.elseBranch; elseBranch) {
        if (NODE_IS(elseBranch.value(), Scope)) {
            auto& scope = NODE_AS_REF(elseBranch.value(), Scope);
            auto scoped{rawPointerize(scope.scoped)};
            elseBlock = generateFromStatement(scoped.begin(), scoped.end());
        } else {
            auto temp = std::vector<const Expression*>{ifSt.elseBranch.value().get()};
            elseBlock = generateFromStatement(temp.begin(), temp.end());
        }
    }
    std::vector<std::unique_ptr<BasicBlock>> post;
    post.push_back(std::move(thenBlock));
    post.push_back(std::move(elseBlock));
    post.push_back(std::move(posterior));
    return std::make_unique<BasicBlock>(BlockType::If, std::move(post),
                                        std::vector<const Expression*>{ifSt.condition.get()});
}

std::unique_ptr<BasicBlock> generateForFuncCall(const Expression& curr, std::unique_ptr<BasicBlock> posterior) {
    const auto& call = static_cast<const FunctionCall&>(curr);
    std::vector<std::unique_ptr<BasicBlock>> post;
    post.push_back(std::move(posterior));
    return std::make_unique<BasicBlock>(
        BlockType::FunctionCall, std::move(post),
        std::vector<const Expression*>{call.name.get(), call.args.has_value() ? call.args.value().get() : nullptr});
}

std::unique_ptr<BasicBlock> generateFromStatement(std::vector<const Expression*>::iterator statementsBegin,
                                                  std::vector<const Expression*>::iterator statementsEnd) {
    if (statementsBegin == statementsEnd)
        return std::make_unique<BasicBlock>(BlockType::FunctionEpilogue, std::vector<std::unique_ptr<BasicBlock>>{});
    auto& top = *statementsBegin;
    switch (top->getType()) {
    case ExpressionType::If:
        return generateForIf(*top, generateFromStatement(++statementsBegin, statementsEnd));
    case ExpressionType::While:
        return generateForWhile(*top, generateFromStatement(++statementsBegin, statementsEnd));
    case ExpressionType::FunctionCall:
        return generateForFuncCall(*top, generateFromStatement(++statementsBegin, statementsEnd));
    default:
        std::vector<std::unique_ptr<BasicBlock>> posterior{};
        std::vector<const Expression*> nodes{};
        for (auto st = statementsBegin; statementsBegin != statementsEnd; ++st = statementsBegin) {
            auto type = (*st)->getType();
            if (type == ExpressionType::If || type == ExpressionType::While || type == ExpressionType::FunctionCall) {
                break;
            }
            nodes.push_back(*st);
            ++statementsBegin;
        }
        posterior.push_back(generateFromStatement(statementsBegin, statementsEnd));
        return std::make_unique<BasicBlock>(BlockType::Normal, std::move(posterior));
    }
}

CFG generateCFG(AST ast) {
    CFG cfg{};
    for (auto& fawFunc : ast.takeTopLevel()) {
        auto& function = NODE_AS_REF(fawFunc, Function);
        auto& statements = function.body;
    }
}