#include "CFG.h"
#include <optional>
#include <ranges>
#include <span>

std::shared_ptr<BasicBlock> generateFromStatement(std::vector<const Expression*>::iterator statementsBegin,
                                                  std::vector<const Expression*>::iterator statementsEnd);

template <typename T> std::vector<const T*> rawPointerize(const std::vector<std::unique_ptr<T>>& vec) {
    std::vector<const T*> ptrs(vec.size());
    std::transform(vec.begin(), vec.end(), ptrs.begin(), [](const auto& ptr) { return ptr.get(); });
    return ptrs;
}

std::shared_ptr<BasicBlock> generateForWhile(const Expression& curr, std::shared_ptr<BasicBlock> posterior) {
    const While& whileSt = static_cast<const While&>(curr);

    std::shared_ptr<BasicBlock> subBlock = nullptr;
    if (NODE_IS(whileSt.body, Scope)) {
        auto& scope = NODE_AS_REF(whileSt.body, Scope);
        auto scoped{rawPointerize(scope.scoped)};
        subBlock = generateFromStatement(scoped.begin(), scoped.end());
    } else {
        std::vector<const Expression*> temp{whileSt.body.get()};
        subBlock = generateFromStatement(temp.begin(), temp.end());
    }

    std::vector<std::shared_ptr<BasicBlock>> post;
    post.push_back(std::move(subBlock));
    post.push_back(std::move(posterior));
    auto whileBlock = std::make_shared<BasicBlock>(BlockType::While, std::move(post),
                                                   std::vector<const Expression*>{whileSt.condition.get()});
    whileBlock->posterior.at(0)->replaceByIf(whileBlock,
                                             [](const BasicBlock& b) { return b.type == BlockType::FunctionEpilogue; });
    std::for_each(whileBlock->posterior.begin(), whileBlock->posterior.end(),
                  [&whileBlock](auto& post) { post->predecessors.push_back(whileBlock.get()); });
    return whileBlock;
}

std::shared_ptr<BasicBlock> generateForIf(const Expression& curr, std::shared_ptr<BasicBlock> posterior) {
    const If& ifSt = static_cast<const If&>(curr);
    std::shared_ptr<BasicBlock> thenBlock = nullptr;
    if (NODE_IS(ifSt.thenBranch, Scope)) {
        auto& scope = NODE_AS_REF(ifSt.thenBranch, Scope);
        auto ptrs = rawPointerize(scope.scoped);
        thenBlock = generateFromStatement(ptrs.begin(), ptrs.end());
    } else {
        auto temp = std::vector<const Expression*>{ifSt.thenBranch.get()};
        thenBlock = generateFromStatement(temp.begin(), temp.end());
    }
    if (thenBlock->type == BlockType::FunctionEpilogue)
        thenBlock = posterior;
    else
        thenBlock->replaceByIf(posterior, [](const BasicBlock& b) { return b.type == BlockType::FunctionEpilogue; });

    std::shared_ptr<BasicBlock> elseBlock = nullptr;
    if (auto& elseBranch = ifSt.elseBranch; elseBranch) {
        if (NODE_IS(elseBranch.value(), Scope)) {
            auto& scope = NODE_AS_REF(elseBranch.value(), Scope);
            auto scoped{rawPointerize(scope.scoped)};
            elseBlock = generateFromStatement(scoped.begin(), scoped.end());
        } else {
            auto temp = std::vector<const Expression*>{ifSt.elseBranch.value().get()};
            elseBlock = generateFromStatement(temp.begin(), temp.end());
        }
        if (elseBlock->type == BlockType::FunctionEpilogue)
            elseBlock = posterior;
        else
            elseBlock->replaceByIf(posterior,
                                   [](const BasicBlock& b) { return b.type == BlockType::FunctionEpilogue; });
    } else {
        elseBlock = posterior;
    }
    std::vector<std::shared_ptr<BasicBlock>> post;
    post.push_back(std::move(thenBlock));
    post.push_back(std::move(elseBlock));

    auto ifBlock = std::make_shared<BasicBlock>(BlockType::If, std::move(post),
                                                std::vector<const Expression*>{ifSt.condition.get()});
    std::for_each(ifBlock->posterior.begin(), ifBlock->posterior.end(),
                  [&ifBlock](auto& post) { post->predecessors.push_back(ifBlock.get()); });
    return ifBlock;
}

std::shared_ptr<BasicBlock> generateForFuncCall(const Expression& curr, std::shared_ptr<BasicBlock> posterior) {
    const auto& call = static_cast<const FunctionCall&>(curr);
    std::vector<std::shared_ptr<BasicBlock>> post;
    post.push_back(std::move(posterior));
    auto funcCallBlock = std::make_shared<BasicBlock>(
        BlockType::FunctionCall, std::move(post),
        std::vector<const Expression*>{call.name.get(), call.args.has_value() ? call.args.value().get() : nullptr});

    std::for_each(funcCallBlock->posterior.begin(), funcCallBlock->posterior.end(),
                  [&funcCallBlock](auto& post) { post->predecessors.push_back(funcCallBlock.get()); });
    return funcCallBlock;
}

std::shared_ptr<BasicBlock> generateforReturn(const Expression& returnEx) {
    auto& casted = static_cast<const Return&>(returnEx);
    std::vector<const Expression*> extraInfo;
    extraInfo.push_back(casted.what ? casted.what.value().get() : nullptr);
    std::vector<std::shared_ptr<BasicBlock>> post{};
    auto returnBlock = std::make_shared<BasicBlock>(BlockType::Return, std::move(post), std::move(extraInfo));

    std::for_each(returnBlock->posterior.begin(), returnBlock->posterior.end(),
                  [&returnBlock](auto& post) { post->predecessors.push_back(returnBlock.get()); });
    return returnBlock;
}

std::shared_ptr<BasicBlock> generateFromStatement(std::vector<const Expression*>::iterator statementsBegin,
                                                  std::vector<const Expression*>::iterator statementsEnd) {
    if (statementsBegin == statementsEnd)
        return std::make_shared<BasicBlock>(BlockType::FunctionEpilogue, std::vector<std::shared_ptr<BasicBlock>>{});
    auto& top = *statementsBegin;
    switch (top->getType()) {
    case ExpressionType::If:
        return generateForIf(*top, generateFromStatement(++statementsBegin, statementsEnd));
    case ExpressionType::While:
        return generateForWhile(*top, generateFromStatement(++statementsBegin, statementsEnd));
    case ExpressionType::FunctionCall:
        return generateForFuncCall(*top, generateFromStatement(++statementsBegin, statementsEnd));
    case ExpressionType::Return:
        return generateforReturn(*top);
    default:
        std::vector<std::shared_ptr<BasicBlock>> posterior{};
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
        auto statementSeq = std::make_shared<BasicBlock>(BlockType::Normal, std::move(posterior), std::move(nodes));
        statementSeq->posterior.at(0)->predecessors.push_back(statementSeq.get());
        return statementSeq;
    }
}

CFG generateCFG(const AST& ast) {
    CFG cfg{};
    std::cout << "CFG generation..." << std::endl;
    for (const auto& fawFunc : ast.getTopLevel()) {
        const auto& function = NODE_AS_REF(fawFunc, Function);
        auto& statements = function.body;
        auto ptrised{rawPointerize(statements)};

        std::vector<std::shared_ptr<BasicBlock>> inner{};
        inner.push_back(generateFromStatement(ptrised.begin(), ptrised.end()));
        auto args = std::vector<const Expression*>{(function.argList ? function.argList.value().get() : nullptr)};
        BasicBlock b(BlockType::FunctionPrologue, std::move(inner), std::move(args));
        std::cout << NODE_AS_REF(function.name, Name).literal << " : \n";
        b.print();
        cfg.functions.emplace(std::pair{NODE_AS_REF(function.name, Name).literal, std::move(b)});
    }
    return cfg;
}

void BasicBlock::replaceByIf(std::shared_ptr<BasicBlock> repl, std::function<bool(const BasicBlock&)> pred) {
    std::cout << posterior.size() << std::endl;
    for (size_t i = 0; i < posterior.size(); ++i) {
        if (posterior[i] != nullptr) {
            if (pred(*posterior[i])) {
                posterior[i] = repl;
                posterior[i]->predecessors.push_back(this);
            } else {
                posterior[i]->replaceByIf(repl, pred);
            }
        }
    }
}

void printIf(const BasicBlock& ifBlock, const BasicBlock* until) {
    std::cout << "if()" << std::endl;
    std::cout << "then->";
    if (ifBlock.posterior.at(0).get() != until)
        ifBlock.posterior.at(0)->print();
    std::cout << "\nelse ->";
    if (ifBlock.posterior.at(1).get() != until)
        ifBlock.posterior.at(1)->print();
}

void printWhile(const BasicBlock& whileBlock, const BasicBlock* until) {
    std::cout << "while()" << std::endl;
    std::cout << "{\n";
    if (whileBlock.posterior.at(0).get() != until)
        whileBlock.posterior.at(0)->print(&whileBlock);
    std::cout << "}\n";
    if (whileBlock.posterior.at(1).get() != until)
        whileBlock.posterior.at(1)->print();
}

void printStatementSeq(const BasicBlock& statementSeq, const BasicBlock* until) {
    for (const auto& node : statementSeq.extraInfo) {
        std::cout << node->toString() << " \n";
    }
    std::cout << "\n ->\n ";
    if (statementSeq.posterior.at(0).get() != until)
        statementSeq.posterior.at(0)->print();
}

void printCall(const BasicBlock& call, const BasicBlock* until) {
    std::cout << call.extraInfo.at(0)->toString() << " ()\n";
    if (call.posterior.at(0).get() != until)
        call.posterior.at(0)->print();
}

void BasicBlock::print(const BasicBlock* until) {
    if (until != nullptr && this == until)
        return;
    switch (type) {
    case BlockType::If:
        printIf(*this, until);
        break;
    case BlockType::While:
        printWhile(*this, until);
        break;
    case BlockType::Normal:
        printStatementSeq(*this, until);
        break;
    case BlockType::FunctionCall:
        printCall(*this, until);
        break;
    case BlockType::FunctionPrologue:
        std::cout << "Function start:" << std::endl;
        posterior.at(0)->print(until);
        break;
    case BlockType::FunctionEpilogue:
        std::cout << "Function end. " << "\n---------------------" << std::endl;
        break;
    }
}
