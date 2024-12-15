#include "InstructionSelector.h"
#include "llvm/IR/Verifier.h"
#include <cstdint>
#include <iostream>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Module.h>
#include <ranges>
#include <unordered_map>
#include <unordered_set>

namespace {
struct pairhash {
  public:
    template <typename T, typename U> std::size_t operator()(const std::pair<T, U>& x) const {
        return std::hash<T>()(x.first) ^ std::hash<U>()(x.second);
    }
};

std::unordered_map<llvm::Value*, std::unordered_set<std::pair<llvm::Value*, size_t>,pairhash>> correctImmediates;

std::uint64_t getCondCode(llvm::CmpInst* cmpInst) {
    switch (cmpInst->getPredicate()) {
    case llvm::ICmpInst::ICMP_EQ:
        return 4;
    case llvm::ICmpInst::ICMP_NE:
        return 5;
    case llvm::ICmpInst::ICMP_SLT:
        return 12;
    case llvm::ICmpInst::ICMP_SGT:
        return 15;
    default:
        cmpInst->print(llvm::errs());
        throw std::runtime_error("unknown icmp");
    }
}

std::uint8_t getBytesFromIntType(llvm::Type* type) {
    if (dyn_cast<llvm::PointerType>(type)) {
        return 8; // 64 bit
    }
    assert(dyn_cast<llvm::IntegerType>(type));
    auto* intType = cast<llvm::IntegerType>(type);
    assert(intType->getBitWidth() % 8 == 0);
    return intType->getBitWidth() / 8;
}

std::pair<std::int64_t, std::int64_t> getMinMaxValWithByteRange(std::uint8_t bytes) {
    switch (bytes) {
    case 1:
        return {INT8_MIN, INT8_MAX};
    case 2:
        return {INT16_MIN, INT16_MAX};
    case 4:
        return {INT32_MIN, INT32_MAX};
    case 8:
        return {INT64_MIN, INT64_MAX};
    }
    throw std::runtime_error("wrong byte value");
}

constexpr bool notConst(llvm::Value* v) { return !dyn_cast<llvm::ConstantInt>(v); }
constexpr bool isConst(llvm::Value* v) { return nullptr != dyn_cast<llvm::ConstantInt>(v); }
constexpr bool isConstAndFits(llvm::Value* v) {
    if (auto* intVal = dyn_cast<llvm::ConstantInt>(v)) {
        auto val = intVal->getSExtValue();
        return val >= INT32_MIN && val <= INT32_MAX;
    }
    return false;
}

struct Pattern {
    virtual bool matches(llvm::Value* root) = 0;
    virtual void markCovered(llvm::Value* root, std::unordered_set<llvm::Value*>& covered) = 0;
    virtual void replace(llvm::Module& module, llvm::Value* root, llvm::Value* parent) = 0;
    virtual std::uint16_t getSize() = 0;
    virtual ~Pattern() = default;
};

static constexpr llvm::Function* getInstruction(llvm::Module& module, llvm::StringRef name, llvm::Type* retType,
                                                std::vector<llvm::Type*> args, bool varags = false) {
    auto* calledFunc = module.getFunction(name);
    if (!calledFunc) {
        // implicit function declaration
        auto type = llvm::FunctionType::get(retType, args, varags);
        calledFunc = llvm::cast<llvm::Function>(module.getOrInsertFunction(name, type).getCallee());
    }
    return calledFunc;
}

struct SHL64rr : public Pattern {
    bool matches(llvm::Value* root) override {
        if (auto* rootInst = dyn_cast<llvm::Instruction>(root)) {
            if (rootInst->getOpcode() == llvm::Instruction::Shl) {
                return true;
            }
        }
        return false;
    }
    void markCovered(llvm::Value* root, std::unordered_set<llvm::Value*>& covered) override { covered.insert(root); }

    void replace(llvm::Module& module, llvm::Value* rootVal, llvm::Value* parent) override {
        assert(dyn_cast<llvm::Instruction>(rootVal));
        auto* root = cast<llvm::Instruction>(rootVal);
        auto* call = llvm::IRBuilder<>{root}.CreateCall(getInstruction(module, "SHL64rr",
                                                                       llvm::Type::getInt64Ty(module.getContext()),
                                                                       {
                                                                           llvm::Type::getInt64Ty(module.getContext()),
                                                                           llvm::Type::getInt64Ty(module.getContext()),
                                                                       }),
                                                        {root->getOperand(0), root->getOperand(1)});
        root->replaceAllUsesWith(call);
        root->eraseFromParent();
    }
    std::uint16_t getSize() override { return 1; }
};

struct SHL64ri : public Pattern {
    bool matches(llvm::Value* rootVal) override {
        if (auto* root = dyn_cast<llvm::Instruction>(rootVal)) {
            if (root->getOpcode() == llvm::Instruction::Shl) {
                // exactly one of them is const
                if (!(notConst(root->getOperand(0)) != notConst(root->getOperand(1)))) {
                    return false;
                }
                llvm::Value* immediate = root->getOperand(0);
                if (notConst(immediate)) {
                    immediate = root->getOperand(1);
                }
                assert(dyn_cast<llvm::ConstantInt>(immediate));
                auto* intVal = cast<llvm::ConstantInt>(immediate);
                auto rawVal = intVal->getSExtValue();
                return rawVal >= INT32_MIN && rawVal <= INT32_MAX;
            }
        }
        return false;
    }
    void markCovered(llvm::Value* rootVal, std::unordered_set<llvm::Value*>& covered) override {
        assert(dyn_cast<llvm::Instruction>(rootVal));
        auto* root = cast<llvm::Instruction>(rootVal);
        covered.insert(root);
        if (isConst(root->getOperand(0))) {
            covered.insert(root->getOperand(0));
        } else {
            covered.insert(root->getOperand(1));
        }
    }
    void replace(llvm::Module& module, llvm::Value* rootVal, llvm::Value* parent) override {
        assert(dyn_cast<llvm::Instruction>(rootVal));
        auto* root = cast<llvm::Instruction>(rootVal);
        llvm::Value* immediate = root->getOperand(0);
        llvm::Value* registerVal = root->getOperand(1);
        if (notConst(immediate)) {
            std::swap(immediate, registerVal);
        }
        auto* call = llvm::IRBuilder<>{root}.CreateCall(getInstruction(module, "SHL64ri",
                                                                       llvm::Type::getInt64Ty(module.getContext()),
                                                                       {
                                                                           llvm::Type::getInt64Ty(module.getContext()),
                                                                           llvm::Type::getInt64Ty(module.getContext()),
                                                                       }),
                                                        {root->getOperand(0), root->getOperand(1)});
        correctImmediates[call].emplace(call->getOperand(1), 1);
        root->replaceAllUsesWith(call);
        root->eraseFromParent();
    }
    std::uint16_t getSize() override { return 2; }
};

struct SAR64rr : public Pattern {
    bool matches(llvm::Value* root) override {
        if (auto* rootInst = dyn_cast<llvm::Instruction>(root)) {
            if (rootInst->getOpcode() == llvm::Instruction::AShr) {
                return true;
            }
        }
        return false;
    }
    void markCovered(llvm::Value* root, std::unordered_set<llvm::Value*>& covered) override { covered.insert(root); }

    void replace(llvm::Module& module, llvm::Value* rootVal, llvm::Value* parent) override {
        assert(dyn_cast<llvm::Instruction>(rootVal));
        auto* root = cast<llvm::Instruction>(rootVal);
        auto* call = llvm::IRBuilder<>{root}.CreateCall(getInstruction(module, "SAR64rr",
                                                                       llvm::Type::getInt64Ty(module.getContext()),
                                                                       {
                                                                           llvm::Type::getInt64Ty(module.getContext()),
                                                                           llvm::Type::getInt64Ty(module.getContext()),
                                                                       }),
                                                        {root->getOperand(0), root->getOperand(1)});
        root->replaceAllUsesWith(call);
        root->eraseFromParent();
    }
    std::uint16_t getSize() override { return 1; }
};

struct SAR64ri : public Pattern {
    bool matches(llvm::Value* rootVal) override {
        if (auto* root = dyn_cast<llvm::Instruction>(rootVal)) {
            if (root->getOpcode() == llvm::Instruction::AShr) {
                // exactly one of them is const
                if (!(notConst(root->getOperand(0)) != notConst(root->getOperand(1)))) {
                    return false;
                }
                llvm::Value* immediate = root->getOperand(0);
                if (notConst(immediate)) {
                    immediate = root->getOperand(1);
                }
                assert(dyn_cast<llvm::ConstantInt>(immediate));
                auto* intVal = cast<llvm::ConstantInt>(immediate);
                auto rawVal = intVal->getSExtValue();
                return rawVal >= INT32_MIN && rawVal <= INT32_MAX;
            }
        }
        return false;
    }
    void markCovered(llvm::Value* rootVal, std::unordered_set<llvm::Value*>& covered) override {
        assert(dyn_cast<llvm::Instruction>(rootVal));
        auto* root = cast<llvm::Instruction>(rootVal);
        covered.insert(root);
        if (isConst(root->getOperand(0))) {
            covered.insert(root->getOperand(0));
        } else {
            covered.insert(root->getOperand(1));
        }
    }
    void replace(llvm::Module& module, llvm::Value* rootVal, llvm::Value* parent) override {
        assert(dyn_cast<llvm::Instruction>(rootVal));
        auto* root = cast<llvm::Instruction>(rootVal);
        llvm::Value* immediate = root->getOperand(0);
        llvm::Value* registerVal = root->getOperand(1);
        if (notConst(immediate)) {
            std::swap(immediate, registerVal);
        }
        auto* call = llvm::IRBuilder<>{root}.CreateCall(getInstruction(module, "SAR64ri",
                                                                       llvm::Type::getInt64Ty(module.getContext()),
                                                                       {
                                                                           llvm::Type::getInt64Ty(module.getContext()),
                                                                           llvm::Type::getInt64Ty(module.getContext()),
                                                                       }),
                                                        {root->getOperand(0), root->getOperand(1)});
        correctImmediates[call].emplace(root->getOperand(1), 1);
        root->replaceAllUsesWith(call);
        root->eraseFromParent();
    }
    std::uint16_t getSize() override { return 2; }
};

struct ADD64rr : public Pattern {
    bool matches(llvm::Value* root) override {
        if (auto* rootInst = dyn_cast<llvm::Instruction>(root)) {
            if (rootInst->getOpcode() == llvm::Instruction::Add) {
                return true;
            }
        }
        return false;
    }
    void markCovered(llvm::Value* root, std::unordered_set<llvm::Value*>& covered) override { covered.insert(root); }

    void replace(llvm::Module& module, llvm::Value* rootVal, llvm::Value* parent) override {
        assert(dyn_cast<llvm::Instruction>(rootVal));
        auto* root = cast<llvm::Instruction>(rootVal);
        auto* call = llvm::IRBuilder<>{root}.CreateCall(getInstruction(module, "ADD64rr",
                                                                       llvm::Type::getInt64Ty(module.getContext()),
                                                                       {
                                                                           llvm::Type::getInt64Ty(module.getContext()),
                                                                           llvm::Type::getInt64Ty(module.getContext()),
                                                                       }),
                                                        {root->getOperand(0), root->getOperand(1)});
        root->replaceAllUsesWith(call);
        root->eraseFromParent();
    }
    std::uint16_t getSize() override { return 1; } // just the plus
};

struct LEA_instead_of_add_mul : public Pattern {
    static bool isFittingConstVal(llvm::Value* v) {
        auto* constVal = dyn_cast<llvm::ConstantInt>(v);
        if (!constVal)
            return false;
        return constVal->getSExtValue() == 1 || constVal->getSExtValue() == 2 || constVal->getSExtValue() == 4 ||
               constVal->getSExtValue() == 8;
    }

    static bool isFittingMul(llvm::Value* v) {
        auto* mulInst = dyn_cast<llvm::Instruction>(v);
        if (mulInst && mulInst->getOpcode() == llvm::Instruction::Mul) {
            return isFittingConstVal(mulInst->getOperand(0)) || isFittingConstVal(mulInst->getOperand(1));
        }
        return false;
    }

    bool matches(llvm::Value* root) override {
        if (auto* rootInst = dyn_cast<llvm::Instruction>(root)) {
            if (rootInst->getOpcode() == llvm::Instruction::Add) {
                return (rootInst->getOperand(0) != rootInst->getOperand(1)) &&
                       (isFittingMul(rootInst->getOperand(0)) || isFittingMul(rootInst->getOperand(1)));
            }
        }
        return false;
    }

    void markCovered(llvm::Value* root, std::unordered_set<llvm::Value*>& covered) override {
        covered.insert(root);
        auto* addInst = cast<llvm::Instruction>(root);
        llvm::Instruction* mulInst = nullptr;
        if (isFittingMul(addInst->getOperand(0))) {
            mulInst = cast<llvm::Instruction>(addInst->getOperand(0));
        } else {
            mulInst = cast<llvm::Instruction>(addInst->getOperand(1));
        }
        covered.insert(mulInst);
        covered.insert(isFittingConstVal(mulInst->getOperand(0)) ? mulInst->getOperand(0) : mulInst->getOperand(1));
    }

    void replace(llvm::Module& module, llvm::Value* rootVal, llvm::Value* parent) override {
        assert(dyn_cast<llvm::Instruction>(rootVal));
        auto* root = cast<llvm::Instruction>(rootVal);

        auto* mulSide =
            cast<llvm::Instruction>(isFittingMul(root->getOperand(0)) ? root->getOperand(0) : root->getOperand(1));
        auto* nonMulSide = (root->getOperand(0) == mulSide) ? root->getOperand(1) : root->getOperand(0);
        assert(mulSide != nonMulSide);
        assert(mulSide->getOpcode() == llvm::Instruction::Mul);

        auto* immediate = llvm::cast<llvm::ConstantInt>(
            isFittingConstVal(mulSide->getOperand(0)) ? mulSide->getOperand(0) : mulSide->getOperand(1));
        auto* mulNonIMmediate = (mulSide->getOperand(0) == immediate) ? mulSide->getOperand(1) : mulSide->getOperand(0);
        assert(immediate != mulNonIMmediate);

        auto* lea = llvm::IRBuilder<>{root}.CreateCall(
            getInstruction(module, "LEA64rm", llvm::Type::getInt64Ty(module.getContext()),
                           {
                               llvm::Type::getInt64Ty(module.getContext()),
                               llvm::Type::getInt64Ty(module.getContext()),
                               llvm::Type::getInt64Ty(module.getContext()),
                               llvm::Type::getInt64Ty(module.getContext()),
                           }),
            {nonMulSide, immediate, mulNonIMmediate,
             llvm::ConstantInt::get(llvm::Type::getInt64Ty(module.getContext()), 0)});

        correctImmediates[lea].emplace(lea->getOperand(1), 1);
        correctImmediates[lea].emplace(lea->getOperand(3), 3);
        root->replaceAllUsesWith(lea);
        root->eraseFromParent();
        mulSide->eraseFromParent();
    }
    std::uint16_t getSize() override { return 3; } // a + (b*c)
};

struct ADD64ri : public Pattern {
    bool matches(llvm::Value* rootVal) override {
        if (auto* root = dyn_cast<llvm::Instruction>(rootVal)) {
            if (root->getOpcode() == llvm::Instruction::Add) {
                // exactly one of them is const
                if (!(notConst(root->getOperand(0)) != notConst(root->getOperand(1)))) {
                    return false;
                }
                llvm::Value* immediate = root->getOperand(0);
                if (notConst(immediate)) {
                    immediate = root->getOperand(1);
                }
                assert(dyn_cast<llvm::ConstantInt>(immediate));
                auto* intVal = cast<llvm::ConstantInt>(immediate);
                auto rawVal = intVal->getSExtValue();
                return rawVal >= INT32_MIN && rawVal <= INT32_MAX;
            }
        }
        return false;
    }
    void markCovered(llvm::Value* rootVal, std::unordered_set<llvm::Value*>& covered) override {
        assert(dyn_cast<llvm::Instruction>(rootVal));
        auto* root = cast<llvm::Instruction>(rootVal);
        covered.insert(root);
        if (isConst(root->getOperand(0))) {
            covered.insert(root->getOperand(0));
        } else {
            covered.insert(root->getOperand(1));
        }
    }
    void replace(llvm::Module& module, llvm::Value* rootVal, llvm::Value* parent) override {
        assert(dyn_cast<llvm::Instruction>(rootVal));
        auto* root = cast<llvm::Instruction>(rootVal);
        llvm::Value* immediate = root->getOperand(0);
        llvm::Value* registerVal = root->getOperand(1);
        if (notConst(immediate)) {
            std::swap(immediate, registerVal);
        }
        auto* call = llvm::IRBuilder<>{root}.CreateCall(getInstruction(module, "ADD64ri",
                                                                       llvm::Type::getInt64Ty(module.getContext()),
                                                                       {
                                                                           llvm::Type::getInt64Ty(module.getContext()),
                                                                           llvm::Type::getInt64Ty(module.getContext()),
                                                                       }),
                                                        {root->getOperand(0), root->getOperand(1)});
        correctImmediates[call].emplace(call->getOperand(1), 1);
        root->replaceAllUsesWith(call);
        root->eraseFromParent();
    }
    std::uint16_t getSize() override { return 2; } // + and one constant
};

struct MUL64rr : public Pattern {
    bool matches(llvm::Value* root) override {
        if (auto* rootInst = dyn_cast<llvm::Instruction>(root)) {
            if (rootInst->getOpcode() == llvm::Instruction::Mul) {
                return true;
            }
        }
        return false;
    }
    void markCovered(llvm::Value* root, std::unordered_set<llvm::Value*>& covered) override { covered.insert(root); }

    void replace(llvm::Module& module, llvm::Value* rootVal, llvm::Value* parent) override {
        assert(dyn_cast<llvm::Instruction>(rootVal));
        auto* root = cast<llvm::Instruction>(rootVal);
        auto* call = llvm::IRBuilder<>{root}.CreateCall(getInstruction(module, "IMUL64rr",
                                                                       llvm::Type::getInt64Ty(module.getContext()),
                                                                       {
                                                                           llvm::Type::getInt64Ty(module.getContext()),
                                                                           llvm::Type::getInt64Ty(module.getContext()),
                                                                       }),
                                                        {root->getOperand(0), root->getOperand(1)});
        root->replaceAllUsesWith(call);
        root->eraseFromParent();
    }
    std::uint16_t getSize() override { return 1; }
};

struct MUL64ri : public Pattern {
    bool matches(llvm::Value* rootVal) override {
        if (auto* root = dyn_cast<llvm::Instruction>(rootVal)) {
            if (root->getOpcode() == llvm::Instruction::Mul) {
                // exactly one of them is const
                if (!(notConst(root->getOperand(0)) != notConst(root->getOperand(1)))) {
                    return false;
                }
                llvm::Value* immediate = root->getOperand(0);
                if (notConst(immediate)) {
                    immediate = root->getOperand(1);
                }
                assert(dyn_cast<llvm::ConstantInt>(immediate));
                return isConstAndFits(immediate);
            }
        }
        return false;
    }
    void markCovered(llvm::Value* rootVal, std::unordered_set<llvm::Value*>& covered) override {
        assert(dyn_cast<llvm::Instruction>(rootVal));
        auto* root = cast<llvm::Instruction>(rootVal);
        covered.insert(root);
        if (isConst(root->getOperand(0))) {
            covered.insert(root->getOperand(0));
        } else {
            covered.insert(root->getOperand(1));
        }
    }
    void replace(llvm::Module& module, llvm::Value* rootVal, llvm::Value* parent) override {
        assert(dyn_cast<llvm::Instruction>(rootVal));
        auto* root = cast<llvm::Instruction>(rootVal);
        llvm::Value* immediate = root->getOperand(0);
        llvm::Value* registerVal = root->getOperand(1);
        if (notConst(immediate)) {
            std::swap(immediate, registerVal);
        }
        auto* call = llvm::IRBuilder<>{root}.CreateCall(getInstruction(module, "IMUL64rri",
                                                                       llvm::Type::getInt64Ty(module.getContext()),
                                                                       {
                                                                           llvm::Type::getInt64Ty(module.getContext()),
                                                                           llvm::Type::getInt64Ty(module.getContext()),
                                                                       }),
                                                        {root->getOperand(0), root->getOperand(1)});
        correctImmediates[call].emplace(call->getOperand(1), 1);

        root->replaceAllUsesWith(call);
        root->eraseFromParent();
    }
    std::uint16_t getSize() override { return 2; }
};

struct And64rr : public Pattern {
    bool matches(llvm::Value* root) override {
        if (auto* rootInst = dyn_cast<llvm::Instruction>(root)) {
            if (rootInst->getOpcode() == llvm::Instruction::And) {
                return true;
            }
        }
        return false;
    }
    void markCovered(llvm::Value* root, std::unordered_set<llvm::Value*>& covered) override { covered.insert(root); }

    void replace(llvm::Module& module, llvm::Value* rootVal, llvm::Value* parent) override {
        assert(dyn_cast<llvm::Instruction>(rootVal));
        auto* root = cast<llvm::Instruction>(rootVal);
        auto* call = llvm::IRBuilder<>{root}.CreateCall(getInstruction(module, "AND64rr",
                                                                       llvm::Type::getInt64Ty(module.getContext()),
                                                                       {
                                                                           llvm::Type::getInt64Ty(module.getContext()),
                                                                           llvm::Type::getInt64Ty(module.getContext()),
                                                                       }),
                                                        {root->getOperand(0), root->getOperand(1)});
        root->replaceAllUsesWith(call);
        root->eraseFromParent();
    }
    std::uint16_t getSize() override { return 1; }
};

struct And64ri : public Pattern {
    bool matches(llvm::Value* rootVal) override {
        if (auto* root = dyn_cast<llvm::Instruction>(rootVal)) {
            if (root->getOpcode() == llvm::Instruction::Add) {
                // exactly one of them is const
                if (!(notConst(root->getOperand(0)) != notConst(root->getOperand(1)))) {
                    return false;
                }
                llvm::Value* immediate = root->getOperand(0);
                if (notConst(immediate)) {
                    immediate = root->getOperand(1);
                }
                assert(dyn_cast<llvm::ConstantInt>(immediate));
                return isConstAndFits(immediate);
            }
        }
        return false;
    }
    void markCovered(llvm::Value* rootVal, std::unordered_set<llvm::Value*>& covered) override {
        assert(dyn_cast<llvm::Instruction>(rootVal));
        auto* root = cast<llvm::Instruction>(rootVal);
        covered.insert(root);
        if (isConst(root->getOperand(0))) {
            covered.insert(root->getOperand(0));
        } else {
            covered.insert(root->getOperand(1));
        }
    }
    void replace(llvm::Module& module, llvm::Value* rootVal, llvm::Value* parent) override {
        assert(dyn_cast<llvm::Instruction>(rootVal));
        auto* root = cast<llvm::Instruction>(rootVal);
        llvm::Value* immediate = root->getOperand(0);
        llvm::Value* registerVal = root->getOperand(1);
        if (notConst(immediate)) {
            std::swap(immediate, registerVal);
        }
        auto* call = llvm::IRBuilder<>{root}.CreateCall(getInstruction(module, "AND64ri",
                                                                       llvm::Type::getInt64Ty(module.getContext()),
                                                                       {
                                                                           llvm::Type::getInt64Ty(module.getContext()),
                                                                           llvm::Type::getInt64Ty(module.getContext()),
                                                                       }),
                                                        {root->getOperand(0), root->getOperand(1)});
        correctImmediates[call].emplace(call->getOperand(1), 1);
        root->replaceAllUsesWith(call);
        root->eraseFromParent();
    }
    std::uint16_t getSize() override { return 2; }
};

struct Or64rr : public Pattern {
    bool matches(llvm::Value* root) override {
        if (auto* rootInst = dyn_cast<llvm::Instruction>(root)) {
            if (rootInst->getOpcode() == llvm::Instruction::Or) {
                return true;
            }
        }
        return false;
    }
    void markCovered(llvm::Value* root, std::unordered_set<llvm::Value*>& covered) override { covered.insert(root); }

    void replace(llvm::Module& module, llvm::Value* rootVal, llvm::Value* parent) override {
        assert(dyn_cast<llvm::Instruction>(rootVal));
        auto* root = cast<llvm::Instruction>(rootVal);
        auto* call = llvm::IRBuilder<>{root}.CreateCall(getInstruction(module, "OR64rr",
                                                                       llvm::Type::getInt64Ty(module.getContext()),
                                                                       {
                                                                           llvm::Type::getInt64Ty(module.getContext()),
                                                                           llvm::Type::getInt64Ty(module.getContext()),
                                                                       }),
                                                        {root->getOperand(0), root->getOperand(1)});
        root->replaceAllUsesWith(call);
        root->eraseFromParent();
    }
    std::uint16_t getSize() override { return 1; }
};

struct Or64ri : public Pattern {
    bool matches(llvm::Value* rootVal) override {
        if (auto* root = dyn_cast<llvm::Instruction>(rootVal)) {
            if (root->getOpcode() == llvm::Instruction::Or) {
                // exactly one of them is const
                if (!(notConst(root->getOperand(0)) != notConst(root->getOperand(1)))) {
                    return false;
                }
                llvm::Value* immediate = root->getOperand(0);
                if (notConst(immediate)) {
                    immediate = root->getOperand(1);
                }
                assert(dyn_cast<llvm::ConstantInt>(immediate));
                return isConstAndFits(immediate);
            }
        }
        return false;
    }
    void markCovered(llvm::Value* rootVal, std::unordered_set<llvm::Value*>& covered) override {
        assert(dyn_cast<llvm::Instruction>(rootVal));
        auto* root = cast<llvm::Instruction>(rootVal);
        covered.insert(root);
        if (isConst(root->getOperand(0))) {
            covered.insert(root->getOperand(0));
        } else {
            covered.insert(root->getOperand(1));
        }
    }
    void replace(llvm::Module& module, llvm::Value* rootVal, llvm::Value* parent) override {
        assert(dyn_cast<llvm::Instruction>(rootVal));
        auto* root = cast<llvm::Instruction>(rootVal);
        llvm::Value* immediate = root->getOperand(0);
        llvm::Value* registerVal = root->getOperand(1);
        if (notConst(immediate)) {
            std::swap(immediate, registerVal);
        }
        auto* call = llvm::IRBuilder<>{root}.CreateCall(getInstruction(module, "OR64ri",
                                                                       llvm::Type::getInt64Ty(module.getContext()),
                                                                       {
                                                                           llvm::Type::getInt64Ty(module.getContext()),
                                                                           llvm::Type::getInt64Ty(module.getContext()),
                                                                       }),
                                                        {root->getOperand(0), root->getOperand(1)});
        correctImmediates[call].emplace(call->getOperand(1), 1);

        root->replaceAllUsesWith(call);
        root->eraseFromParent();
    }
    std::uint16_t getSize() override { return 2; }
};

struct MOV64ri : public Pattern {
    bool matches(llvm::Value* rootVal) override { return isConst(rootVal) && rootVal->getType()->isIntegerTy(); }
    void markCovered(llvm::Value* rootVal, std::unordered_set<llvm::Value*>& covered) override {
        covered.insert(rootVal);
    }
    void replace(llvm::Module& module, llvm::Value* rootVal, llvm::Value* parent) override {
        // done later at fixup time
    }
    std::uint16_t getSize() override { return 1; } // one constant
};

struct MOV32ri : public Pattern {
    bool matches(llvm::Value* rootVal) override { return isConst(rootVal) && rootVal->getType()->isIntegerTy(32); }
    void markCovered(llvm::Value* rootVal, std::unordered_set<llvm::Value*>& covered) override {
        covered.insert(rootVal);
    }
    void replace(llvm::Module& module, llvm::Value* rootVal, llvm::Value* parent) override {
        assert(parent);
        auto* parentInst = cast<llvm::Instruction>(parent);
        auto* call = llvm::IRBuilder<>{parentInst}.CreateCall(
            getInstruction(module, "MOV32ri", llvm::Type::getInt64Ty(module.getContext()),
                           {
                               llvm::Type::getInt64Ty(module.getContext()),
                           }),
            {rootVal});
        correctImmediates[call].emplace(rootVal, 0);
        for (size_t i = 0; i < parentInst->getNumOperands(); ++i) {
            if (parentInst->getOperand(i) == rootVal) {
                parentInst->setOperand(i, call);
                break;
            }
        }
    }
    std::uint16_t getSize() override { return 1; } // one constant
};

struct SUB64rr : public Pattern {
    bool matches(llvm::Value* root) override {
        if (auto* rootInst = dyn_cast<llvm::Instruction>(root)) {
            if (rootInst->getOpcode() == llvm::Instruction::Sub) {
                return true; // TODO: is this ok?
                //           return notConst(rootInst->getOperand(0)) && notConst(rootInst->getOperand(1)); // else
                //           use imm
            }
        }
        return false;
    }
    void markCovered(llvm::Value* root, std::unordered_set<llvm::Value*>& covered) override { covered.insert(root); }

    void replace(llvm::Module& module, llvm::Value* rootVal, llvm::Value* parent) override {
        auto* root = cast<llvm::Instruction>(rootVal);
        auto* call = llvm::IRBuilder<>{root}.CreateCall(getInstruction(module, "SUB64rr",
                                                                       llvm::Type::getInt64Ty(module.getContext()),
                                                                       {
                                                                           llvm::Type::getInt64Ty(module.getContext()),
                                                                           llvm::Type::getInt64Ty(module.getContext()),
                                                                       }),
                                                        {root->getOperand(0), root->getOperand(1)});
        root->replaceAllUsesWith(call);
        root->eraseFromParent();
    }
    std::uint16_t getSize() override { return 1; } // just the minus
};

struct SUB64ri : public Pattern {
    bool matches(llvm::Value* rootVal) override {
        if (auto* root = dyn_cast<llvm::Instruction>(rootVal)) {
            if (root->getOpcode() == llvm::Instruction::Sub) {
                // exactly one of them is const
                return isConstAndFits(root->getOperand(1));
            }
        }
        return false;
    }
    void markCovered(llvm::Value* rootVal, std::unordered_set<llvm::Value*>& covered) override {
        auto* root = cast<llvm::Instruction>(rootVal);
        covered.insert(root);
        if (isConst(root->getOperand(0)))
            covered.insert(root->getOperand(0));
        else
            covered.insert(root->getOperand(1));
    }
    void replace(llvm::Module& module, llvm::Value* rootVal, llvm::Value* parent) override {
        auto* root = cast<llvm::Instruction>(rootVal);
        llvm::Value* immediate = root->getOperand(1);
        llvm::Value* registerVal = root->getOperand(0);
        auto* call = llvm::IRBuilder<>{root}.CreateCall(getInstruction(module, "SUB64ri",
                                                                       llvm::Type::getInt64Ty(module.getContext()),
                                                                       {
                                                                           llvm::Type::getInt64Ty(module.getContext()),
                                                                           llvm::Type::getInt64Ty(module.getContext()),
                                                                       }),
                                                        {root->getOperand(0), root->getOperand(1)});
        correctImmediates[call].emplace(immediate, 1);
        root->replaceAllUsesWith(call);
        root->eraseFromParent();
    }
    std::uint16_t getSize() override { return 2; }
};

struct XOR64rr : public Pattern {
    bool matches(llvm::Value* root) override {
        if (auto* rootInst = dyn_cast<llvm::Instruction>(root)) {
            if (rootInst->getOpcode() == llvm::Instruction::Xor) {
                return true;
            }
        }
        return false;
    }
    void markCovered(llvm::Value* root, std::unordered_set<llvm::Value*>& covered) override { covered.insert(root); }

    void replace(llvm::Module& module, llvm::Value* rootVal, llvm::Value* parent) override {
        auto* root = cast<llvm::Instruction>(rootVal);
        auto* call = llvm::IRBuilder<>{root}.CreateCall(getInstruction(module, "XOR64rr",
                                                                       llvm::Type::getInt64Ty(module.getContext()),
                                                                       {
                                                                           llvm::Type::getInt64Ty(module.getContext()),
                                                                           llvm::Type::getInt64Ty(module.getContext()),
                                                                       }),
                                                        {root->getOperand(0), root->getOperand(1)});
        root->replaceAllUsesWith(call);
        root->eraseFromParent();
    }
    std::uint16_t getSize() override { return 1; } // just the minus
};

struct XOR64ri : public Pattern {
    bool matches(llvm::Value* rootVal) override {
        if (auto* root = dyn_cast<llvm::Instruction>(rootVal)) {
            if (root->getOpcode() == llvm::Instruction::Xor) {
                // exactly one of them is const
                if (!(notConst(root->getOperand(0)) != notConst(root->getOperand(1)))) {
                    return false;
                }
                llvm::Value* immediate = root->getOperand(0);
                if (notConst(immediate)) {
                    immediate = root->getOperand(1);
                }
                return isConstAndFits(immediate);
            }
        }
        return false;
    }
    void markCovered(llvm::Value* rootVal, std::unordered_set<llvm::Value*>& covered) override {
        auto* root = cast<llvm::Instruction>(rootVal);
        covered.insert(root);
        if (isConst(root->getOperand(0))) {
            covered.insert(root->getOperand(0));
        } else {
            covered.insert(root->getOperand(1));
        }
    }
    void replace(llvm::Module& module, llvm::Value* rootVal, llvm::Value* parent) override {
        auto* root = cast<llvm::Instruction>(rootVal);
        llvm::Value* immediate = root->getOperand(0);
        llvm::Value* registerVal = root->getOperand(1);
        if (notConst(immediate)) {
            std::swap(immediate, registerVal);
        }

        auto* call = llvm::IRBuilder<>{root}.CreateCall(getInstruction(module, "XOR64ri",
                                                                       llvm::Type::getInt64Ty(module.getContext()),
                                                                       {
                                                                           llvm::Type::getInt64Ty(module.getContext()),
                                                                           llvm::Type::getInt64Ty(module.getContext()),
                                                                       }),
                                                        {registerVal, immediate});
        correctImmediates[call].emplace(immediate, 1);
        root->replaceAllUsesWith(call);
        root->eraseFromParent();
    }
    std::uint16_t getSize() override { return 2; }
};

struct Compare64rr : public Pattern {
    bool matches(llvm::Value* root) override {
        if (auto* rootInst = dyn_cast<llvm::Instruction>(root)) {
            if (rootInst->getOpcode() == llvm::Instruction::ICmp) {
                return true;
            }
        }
        return false;
    }
    void markCovered(llvm::Value* root, std::unordered_set<llvm::Value*>& covered) override { covered.insert(root); }

    void replace(llvm::Module& module, llvm::Value* rootVal, llvm::Value* parent) override {
        auto* root = cast<llvm::Instruction>(rootVal);
        auto* cmpInst = cast<llvm::ICmpInst>(rootVal);
        int64_t cond_code = getCondCode(cmpInst);

        llvm::IRBuilder<> builder{root};
        // cmp
        // setcc
        // andi 1
        auto* cmp = builder.CreateCall(getInstruction(module, "CMP64rr", llvm::Type::getVoidTy(module.getContext()),
                                                      {
                                                          llvm::Type::getInt64Ty(module.getContext()),
                                                          llvm::Type::getInt64Ty(module.getContext()),
                                                      }),
                                       {root->getOperand(0), root->getOperand(1)});

        auto* setcc =
            builder.CreateCall(getInstruction(module, "SETcc8r", llvm::Type::getInt64Ty(module.getContext()),
                                              {
                                                  llvm::Type::getInt64Ty(module.getContext()),
                                              }),
                               {llvm::ConstantInt::get(llvm::Type::getInt64Ty(module.getContext()), cond_code)});
        correctImmediates[setcc].emplace(setcc->getOperand(0), 0);
        auto* andInst =
            builder.CreateCall(getInstruction(module, "AND64ri", llvm::Type::getInt64Ty(module.getContext()),
                                              {
                                                  llvm::Type::getInt64Ty(module.getContext()),
                                                  llvm::Type::getInt64Ty(module.getContext()),
                                              }),
                               {setcc, llvm::ConstantInt::get(llvm::Type::getInt64Ty(module.getContext()), 1)});
        correctImmediates[andInst].emplace(andInst->getOperand(1), 1);
        root->replaceAllUsesWith(andInst);
        root->eraseFromParent();
    }
    std::uint16_t getSize() override { return 1; }
};

struct Compare64ri : public Pattern {
    bool matches(llvm::Value* root) override {
        if (auto* rootInst = dyn_cast<llvm::Instruction>(root)) {
            if (rootInst->getOpcode() == llvm::Instruction::ICmp) {
                return isConstAndFits(
                    rootInst->getOperand(1)); // one could also try to handle left one being constant but
                                              // that would involve flipping the comparison operator
            }
        }
        return false;
    }
    void markCovered(llvm::Value* root, std::unordered_set<llvm::Value*>& covered) override { covered.insert(root); }

    void replace(llvm::Module& module, llvm::Value* rootVal, llvm::Value* parent) override {
        auto* root = cast<llvm::Instruction>(rootVal);
        auto* cmpInst = cast<llvm::ICmpInst>(rootVal);
        int64_t cond_code = getCondCode(cmpInst);
        llvm::IRBuilder<> builder{root};
        // cmp
        // setcc
        // andi 1
        auto* cmp = builder.CreateCall(getInstruction(module, "CMP64ri", llvm::Type::getVoidTy(module.getContext()),
                                                      {
                                                          llvm::Type::getInt64Ty(module.getContext()),
                                                          llvm::Type::getInt64Ty(module.getContext()),
                                                      }),
                                       {root->getOperand(0), root->getOperand(1)});
        correctImmediates[cmp].emplace(root->getOperand(1), 1);
        auto* setcc =
            builder.CreateCall(getInstruction(module, "SETcc8r", llvm::Type::getInt64Ty(module.getContext()),
                                              {
                                                  llvm::Type::getInt64Ty(module.getContext()),
                                              }),
                               {llvm::ConstantInt::get(llvm::Type::getInt64Ty(module.getContext()), cond_code)});
        correctImmediates[setcc].emplace(setcc->getOperand(0), 0);
        auto* andInst =
            builder.CreateCall(getInstruction(module, "AND64ri", llvm::Type::getInt64Ty(module.getContext()),
                                              {
                                                  llvm::Type::getInt64Ty(module.getContext()),
                                                  llvm::Type::getInt64Ty(module.getContext()),
                                              }),
                               {setcc, llvm::ConstantInt::get(llvm::Type::getInt64Ty(module.getContext()), 1)});
        correctImmediates[andInst].emplace(andInst->getOperand(1), 1);
        root->replaceAllUsesWith(andInst);
        root->eraseFromParent();
    }
    std::uint16_t getSize() override { return 2; }
};

struct Zext : public Pattern { // TODO: replace by  actual zext
    std::unordered_map<llvm::Value*, std::size_t> bitsExtendedFrom;
    bool matches(llvm::Value* root) override {
        if (auto* rootInst = dyn_cast<llvm::ZExtInst>(root)) {
            assert(rootInst->getDestTy() == llvm::Type::getInt64Ty(rootInst->getContext()));
            if (rootInst->getSrcTy() == llvm::Type::getInt1Ty(rootInst->getContext())) {
                bitsExtendedFrom[rootInst] = 1;
            } else {
                throw std::runtime_error("not implemeneted cast");
            }
            return true;
        }
        return false;
    }
    void markCovered(llvm::Value* root, std::unordered_set<llvm::Value*>& covered) override { covered.insert(root); }

    void replace(llvm::Module& module, llvm::Value* rootVal, llvm::Value* parent) override {
        auto* root = cast<llvm::ZExtInst>(rootVal);

        llvm::IRBuilder<> builder{root};
        // and with (1<< numBitsToKeep) -1

        auto* zext =
            builder.CreateCall(getInstruction(module, "AND64ri", llvm::Type::getVoidTy(module.getContext()),
                                              {
                                                  llvm::Type::getInt64Ty(module.getContext()),
                                                  llvm::Type::getInt64Ty(module.getContext()),
                                              }),
                               {root->getOperand(0), llvm::ConstantInt::get(llvm::Type::getInt64Ty(module.getContext()),
                                                                            (1ull << bitsExtendedFrom[root]) - 1)});
        correctImmediates[zext].emplace(zext->getOperand(1), 1);
        root->replaceAllUsesWith(zext);
        root->eraseFromParent();
    }
    std::uint16_t getSize() override { return 1; }
};

struct Sext : public Pattern {
    std::unordered_map<llvm::Value*, std::size_t> byteExtendedFrom;
    bool matches(llvm::Value* root) override {
        if (auto* rootInst = dyn_cast<llvm::SExtInst>(root)) {
            assert(rootInst->getDestTy() == llvm::Type::getInt64Ty(rootInst->getContext()));
            return true;
        }
        return false;
    }
    void markCovered(llvm::Value* root, std::unordered_set<llvm::Value*>& covered) override {
        covered.insert(root);
        byteExtendedFrom[root] = getBytesFromIntType(cast<llvm::SExtInst>(root)->getSrcTy());
    }

    void replace(llvm::Module& module, llvm::Value* rootVal, llvm::Value* parent) override {
        auto* root = cast<llvm::SExtInst>(rootVal);

        llvm::IRBuilder<> builder{root};

        std::string_view instruction_name = [](std::uint64_t numBytesSource) {
            switch (numBytesSource) {
            case 1:
                return "MOVSXB64rr";
            case 2:
                return "MOVSXW64rr";
            case 4:
                return "MOVSXWD4rr";
            }
            throw std::runtime_error("sext of unknown byte num");
        }(byteExtendedFrom[root]);

        auto* sext =
            builder.CreateCall(getInstruction(module, instruction_name, llvm::Type::getInt64Ty(module.getContext()),
                                              {
                                                  llvm::Type::getInt64Ty(module.getContext()),
                                              }),
                               {root->getOperand(0)});

        root->replaceAllUsesWith(sext);
        root->eraseFromParent();
    }
    std::uint16_t getSize() override { return 1; }
};

struct AllocaDummy : public Pattern {
    bool matches(llvm::Value* root) override {
        if (auto* rootInst = dyn_cast<llvm::AllocaInst>(root)) {
            return true;
        }
        return false;
    }
    void markCovered(llvm::Value* root, std::unordered_set<llvm::Value*>& covered) override { covered.insert(root); }

    void replace(llvm::Module& module, llvm::Value* rootVal, llvm::Value* parent) override {}
    std::uint16_t getSize() override { return 1; }
};

struct CallDummy : public Pattern {
    bool matches(llvm::Value* root) override {
        if (auto* rootInst = dyn_cast<llvm::CallInst>(root)) {
            return true;
        }
        return false;
    }
    void markCovered(llvm::Value* root, std::unordered_set<llvm::Value*>& covered) override { covered.insert(root); }

    void replace(llvm::Module& module, llvm::Value* rootVal, llvm::Value* parent) override {}
    std::uint16_t getSize() override { return 1; }
};

struct PhiDummy : public Pattern {
    bool matches(llvm::Value* root) override {
        if (auto* rootInst = dyn_cast<llvm::PHINode>(root)) {
            return true;
        }
        return false;
    }
    void markCovered(llvm::Value* root, std::unordered_set<llvm::Value*>& covered) override { covered.insert(root); }

    void replace(llvm::Module& module, llvm::Value* rootVal, llvm::Value* parent) override {
        cast<llvm::PHINode>(rootVal)->mutateType(llvm::Type::getInt64Ty(module.getContext()));
    }
    std::uint16_t getSize() override { return 1; }
};

struct PtrToIntDummy : public Pattern {
    bool matches(llvm::Value* root) override {
        if (auto* rootInst = dyn_cast<llvm::PtrToIntInst>(root)) {
            assert(rootInst->getType() == llvm::Type::getInt64Ty(rootInst->getContext()));
            return true;
        }
        return false;
    }
    void markCovered(llvm::Value* root, std::unordered_set<llvm::Value*>& covered) override { covered.insert(root); }

    void replace(llvm::Module& module, llvm::Value* rootVal, llvm::Value* parent) override {
        auto* ptrtoInt = cast<llvm::PtrToIntInst>(rootVal);
        ptrtoInt->replaceAllUsesWith(ptrtoInt->getPointerOperand());
        ptrtoInt->eraseFromParent();
    }
    std::uint16_t getSize() override { return 1; }
};

struct IntToPTrDummy : public Pattern {
    bool matches(llvm::Value* root) override {
        if (auto* rootInst = dyn_cast<llvm::IntToPtrInst>(root)) {
            return true;
        }
        return false;
    }
    void markCovered(llvm::Value* root, std::unordered_set<llvm::Value*>& covered) override { covered.insert(root); }

    void replace(llvm::Module& module, llvm::Value* rootVal, llvm::Value* parent) override {
        auto* intCast = cast<llvm::IntToPtrInst>(rootVal);
        intCast->replaceAllUsesWith(intCast->getOperand(0));
        intCast->eraseFromParent();
    }
    std::uint16_t getSize() override { return 1; }
};

struct Br : public Pattern {
    bool matches(llvm::Value* root) override {
        if (auto* rootInst = dyn_cast<llvm::BranchInst>(root)) {
            return true;
        }
        return false;
    }
    void markCovered(llvm::Value* root, std::unordered_set<llvm::Value*>& covered) override { covered.insert(root); }

    void replace(llvm::Module& module, llvm::Value* rootVal, llvm::Value* parent) override {
        auto* br = cast<llvm::BranchInst>(rootVal);
        if (br->isConditional()) {
            llvm::IRBuilder<> builder{br};
            auto* cmp = builder.CreateCall(
                getInstruction(module, "CMP64ri", llvm::Type::getVoidTy(module.getContext()),
                               {
                                   llvm::Type::getInt64Ty(module.getContext()),
                                   llvm::Type::getInt64Ty(module.getContext()),
                               }),
                {br->getCondition(), llvm::ConstantInt::get(llvm::Type::getInt64Ty(module.getContext()), 0)});
            correctImmediates[cmp].emplace(cmp->getOperand(1), 1);
            auto* jcc = builder.CreateCall(getInstruction(module, "Jcc", llvm::Type::getInt1Ty(module.getContext()),
                                                          {
                                                              llvm::Type::getInt64Ty(module.getContext()),

                                                          }),
                                           {llvm::ConstantInt::get(llvm::Type::getInt64Ty(module.getContext()), 5)});
            br->setCondition(jcc);
            correctImmediates[jcc].emplace(jcc->getOperand(0), 0);
        }
    }
    std::uint16_t getSize() override { return 1; }
};

struct BrAndCompRR : public Pattern {
    bool matches(llvm::Value* root) override {
        if (auto* rootInst = dyn_cast<llvm::BranchInst>(root)) {
            if (!rootInst->isConditional())
                return false;
            return nullptr != dyn_cast<llvm::CmpInst>(rootInst->getCondition());
        }
        return false;
    }
    void markCovered(llvm::Value* root, std::unordered_set<llvm::Value*>& covered) override {
        covered.insert(root);
        covered.insert(dyn_cast<llvm::BranchInst>(root)->getCondition());
    }

    void replace(llvm::Module& module, llvm::Value* rootVal, llvm::Value* parent) override {
        auto* br = cast<llvm::BranchInst>(rootVal);
        llvm::IRBuilder<> builder{br};
        auto* originalCmp = cast<llvm::CmpInst>(br->getCondition());

        auto* cmp = builder.CreateCall(getInstruction(module, "CMP64rr", llvm::Type::getVoidTy(module.getContext()),
                                                      {
                                                          llvm::Type::getInt64Ty(module.getContext()),
                                                          llvm::Type::getInt64Ty(module.getContext()),
                                                      }),
                                       {originalCmp->getOperand(0), originalCmp->getOperand(1)});

        auto* jcc = builder.CreateCall(
            getInstruction(module, "Jcc", llvm::Type::getInt1Ty(module.getContext()),
                           {
                               llvm::Type::getInt64Ty(module.getContext()),

                           }),
            {llvm::ConstantInt::get(llvm::Type::getInt64Ty(module.getContext()), getCondCode(originalCmp))});
        br->setCondition(jcc);
        originalCmp->eraseFromParent();
        correctImmediates[jcc].emplace(jcc->getOperand(0), 0);
    }
    std::uint16_t getSize() override { return 2; }
};

struct BrAndCompRI : public Pattern {
    bool matches(llvm::Value* root) override {
        if (auto* rootInst = dyn_cast<llvm::BranchInst>(root)) {
            if (!rootInst->isConditional())
                return false;
            return nullptr != dyn_cast<llvm::CmpInst>(rootInst->getCondition()) &&
                   isConstAndFits(dyn_cast<llvm::CmpInst>(rootInst->getCondition())->getOperand(1));
        }
        return false;
    }
    void markCovered(llvm::Value* root, std::unordered_set<llvm::Value*>& covered) override {
        covered.insert(root);
        covered.insert(dyn_cast<llvm::BranchInst>(root)->getCondition());
    }

    void replace(llvm::Module& module, llvm::Value* rootVal, llvm::Value* parent) override {
        auto* br = cast<llvm::BranchInst>(rootVal);
        llvm::IRBuilder<> builder{br};
        auto* originalCmp = cast<llvm::CmpInst>(br->getCondition());

        auto* cmp = builder.CreateCall(getInstruction(module, "CMP64ri", llvm::Type::getVoidTy(module.getContext()),
                                                      {
                                                          llvm::Type::getInt64Ty(module.getContext()),
                                                          llvm::Type::getInt64Ty(module.getContext()),
                                                      }),
                                       {originalCmp->getOperand(0), originalCmp->getOperand(1)});
        correctImmediates[cmp].emplace(cmp->getOperand(1), 1);
        auto* jcc = builder.CreateCall(
            getInstruction(module, "Jcc", llvm::Type::getInt1Ty(module.getContext()),
                           {
                               llvm::Type::getInt64Ty(module.getContext()),

                           }),
            {llvm::ConstantInt::get(llvm::Type::getInt64Ty(module.getContext()), getCondCode(originalCmp))});
        br->setCondition(jcc);
        originalCmp->eraseFromParent();
        correctImmediates[jcc].emplace(jcc->getOperand(0), 0);
    }
    std::uint16_t getSize() override { return 3; }
};

struct Loadrm : public Pattern {
    std::unordered_map<llvm::Value*, std::uint64_t> bytesLoadedFrom;
    bool matches(llvm::Value* rootVal) override {
        if (auto* root = dyn_cast<llvm::LoadInst>(rootVal)) {
            return true;
        }
        return false;
    }
    void markCovered(llvm::Value* rootVal, std::unordered_set<llvm::Value*>& covered) override {
        covered.insert(rootVal);
        bytesLoadedFrom[rootVal] = getBytesFromIntType(cast<llvm::LoadInst>(rootVal)->getType());
    }
    void replace(llvm::Module& module, llvm::Value* rootVal, llvm::Value* parent) override {
        auto* root = cast<llvm::LoadInst>(rootVal);

        llvm::IRBuilder<> buider{root};

        std::string_view instruction_name = [](std::uint64_t numBytesSource) {
            switch (numBytesSource) {
            case 1:
                return "MOVZXB32rm";
            case 2:
                return "MOVZXW32rm";
            case 4:
                return "MOV32rm";
            case 8:
                return "MOV64rm";
            }
            throw std::runtime_error("load of unknown byte num");
        }(bytesLoadedFrom[root]);

        auto* load = buider.CreateCall(
            getInstruction(module, instruction_name, llvm::Type::getInt64Ty(module.getContext()),
                           {llvm::Type::getInt64Ty(module.getContext()), llvm::Type::getInt64Ty(module.getContext()),
                            llvm::Type::getInt64Ty(module.getContext()), llvm::Type::getInt64Ty(module.getContext())}),
            {root->getPointerOperand(), llvm::ConstantInt::get(llvm::Type::getInt64Ty(module.getContext()), 8),
             llvm::ConstantInt::get(llvm::Type::getInt64Ty(module.getContext()), 0),
             llvm::ConstantInt::get(llvm::Type::getInt64Ty(module.getContext()), 0)});
        correctImmediates[load].emplace(load->getOperand(1), 1);
        //        correctImmediates[load].emplace(load->getOperand(2),2);
        correctImmediates[load].emplace(load->getOperand(3), 3);
        root->replaceAllUsesWith(load);
        root->eraseFromParent();
    }
    std::uint16_t getSize() override { return 1; }
};

struct Storemr : public Pattern {
    std::unordered_map<llvm::StoreInst*, std::uint8_t> byteStoreMap;
    bool matches(llvm::Value* rootVal) override {
        if (auto* root = dyn_cast<llvm::StoreInst>(rootVal)) {
            return true;
        }
        return false;
    }
    void markCovered(llvm::Value* rootVal, std::unordered_set<llvm::Value*>& covered) override {
        assert(dyn_cast<llvm::StoreInst>(rootVal));
        byteStoreMap[cast<llvm::StoreInst>(rootVal)] =
            getBytesFromIntType(cast<llvm::StoreInst>(rootVal)->getOperand(0)->getType());
        covered.insert(rootVal);
    }
    void replace(llvm::Module& module, llvm::Value* rootVal, llvm::Value* parent) override {
        assert(dyn_cast<llvm::StoreInst>(rootVal));
        auto* root = cast<llvm::StoreInst>(rootVal);

        llvm::IRBuilder<> buider{root};

        std::string_view instr = "";
        switch (byteStoreMap[root]) {
        case 8:
            instr = "MOV64mr";
            break;
        case 4:
            instr = "MOV32mr";
            break;
        case 2:
            instr = "MOV16mr";
            break;
        case 1:
            instr = "MOV8mr";
            break;
        }

        auto* store = buider.CreateCall(
            getInstruction(module, instr, llvm::Type::getVoidTy(module.getContext()),
                           {
                               llvm::Type::getInt64Ty(module.getContext()),
                               llvm::Type::getInt64Ty(module.getContext()),
                               llvm::Type::getInt64Ty(module.getContext()),
                               llvm::Type::getInt64Ty(module.getContext()),
                               llvm::Type::getInt64Ty(module.getContext()),
                           }),
            {root->getPointerOperand(), llvm::ConstantInt::get(llvm::Type::getInt64Ty(module.getContext()), 8),
             llvm::ConstantInt::get(llvm::Type::getInt64Ty(module.getContext()), 0),
             llvm::ConstantInt::get(llvm::Type::getInt64Ty(module.getContext()), 0), root->getOperand(0)});
        correctImmediates[store].emplace(store->getOperand(1), 1);
        // correctImmediates[store].insert(store->getOperand(2));
        correctImmediates[store].emplace(store->getOperand(3), 3);
        root->replaceAllUsesWith(store);
        root->eraseFromParent();
    }
    std::uint16_t getSize() override { return 1; }
};

struct Storemi : public Pattern {
    std::unordered_map<llvm::StoreInst*, std::uint8_t> byteStoreMap;
    std::unordered_map<llvm::StoreInst*, llvm::Value*> immedateStore; // if it is replaced by another pattern ig?
    bool matches(llvm::Value* rootVal) override {
        if (auto* root = dyn_cast<llvm::StoreInst>(rootVal)) {
            return dyn_cast<llvm::ConstantInt>(root->getOperand(0));
        }
        return false;
    }

    void markCovered(llvm::Value* rootVal, std::unordered_set<llvm::Value*>& covered) override {
        assert(dyn_cast<llvm::StoreInst>(rootVal));
        byteStoreMap[cast<llvm::StoreInst>(rootVal)] =
            getBytesFromIntType(cast<llvm::StoreInst>(rootVal)->getOperand(0)->getType());
        covered.insert(rootVal);
        covered.insert(cast<llvm::StoreInst>(rootVal)->getOperand(0));
        immedateStore[cast<llvm::StoreInst>(rootVal)] = cast<llvm::StoreInst>(rootVal)->getOperand(0);
    }

    void replace(llvm::Module& module, llvm::Value* rootVal, llvm::Value* parent) override {
        assert(dyn_cast<llvm::StoreInst>(rootVal));
        auto* root = cast<llvm::StoreInst>(rootVal);
        llvm::IRBuilder<> buider{root};

        std::string_view instr = "";
        switch (byteStoreMap[root]) {
        case 8:
            instr = "MOV64mi";
            break;
        case 4:
            instr = "MOV32mi";
            break;
        case 2:
            instr = "MOV16mi";
            break;
        case 1:
            instr = "MOV8mi";
            break;
        }
        auto* immVal = immedateStore[root];
        std::uint64_t bytes = getBytesFromIntType(immVal->getType());
        //   root->getOperand(0)->print(llvm::errs());
        assert(dyn_cast<llvm::ConstantInt>(immVal));
        std::int64_t val = cast<llvm::ConstantInt>(immVal)->getSExtValue();
        auto [min, max] = getMinMaxValWithByteRange(bytes);
        assert(val >= min && val <= max);

        auto* imm = llvm::ConstantInt::get(llvm::Type::getInt64Ty(module.getContext()), val);
        if (imm->getType() != llvm::Type::getInt64Ty(module.getContext())) {
            imm->mutateType(llvm::Type::getInt64Ty(module.getContext()));
        }

        auto* store = buider.CreateCall(getInstruction(module, instr, llvm::Type::getVoidTy(module.getContext()),
                                                       {
                                                           llvm::Type::getInt64Ty(module.getContext()),
                                                           llvm::Type::getInt64Ty(module.getContext()),
                                                           llvm::Type::getInt64Ty(module.getContext()),
                                                           llvm::Type::getInt64Ty(module.getContext()),
                                                           llvm::Type::getInt64Ty(module.getContext()),
                                                       }),
                                        {root->getPointerOperand(),
                                         llvm::ConstantInt::get(llvm::Type::getInt64Ty(module.getContext()), 8),
                                         llvm::ConstantInt::get(llvm::Type::getInt64Ty(module.getContext()), 0),
                                         llvm::ConstantInt::get(llvm::Type::getInt64Ty(module.getContext()), 0), imm});

        correctImmediates[store].emplace(store->getOperand(1), 1);
        //  correctImmediates[store].insert(store->getOperand(2));
        correctImmediates[store].emplace(store->getOperand(3), 3);
        correctImmediates[store].emplace(store->getOperand(4), 4);

        root->replaceAllUsesWith(store);
        root->eraseFromParent();
    }
    std::uint16_t getSize() override { return 2; }
};

struct GEP : public Pattern {
    std::unordered_map<llvm::GetElementPtrInst*, std::uint8_t> byteStoreMap;

    bool matches(llvm::Value* rootVal) override {
        if (auto* root = dyn_cast<llvm::GetElementPtrInst>(rootVal)) {
            return true;
        }
        return false;
    }
    void markCovered(llvm::Value* rootVal, std::unordered_set<llvm::Value*>& covered) override {
        byteStoreMap[cast<llvm::GetElementPtrInst>(rootVal)] =
            getBytesFromIntType((*cast<llvm::GetElementPtrInst>(rootVal)->idx_begin())->getType());
        covered.insert(rootVal);
    }
    void replace(llvm::Module& module, llvm::Value* rootVal, llvm::Value* parent) override {
        auto* root = cast<llvm::GetElementPtrInst>(rootVal);
        assert(root->getNumIndices() == 1);
        llvm::IRBuilder<> builder{root};

        auto ptr = builder.CreateCall(
            getInstruction(module, "LEA64rm", llvm::Type::getInt64Ty(module.getContext()),
                           {
                               llvm::Type::getInt64Ty(module.getContext()),
                               llvm::Type::getInt64Ty(module.getContext()),
                               llvm::Type::getInt64Ty(module.getContext()),
                               llvm::Type::getInt64Ty(module.getContext()),
                           }),
            {root->getPointerOperand(),
             llvm::ConstantInt::get(llvm::Type::getInt64Ty(module.getContext()), byteStoreMap[root]),
             *root->idx_begin(), llvm::ConstantInt::get(llvm::Type::getInt64Ty(module.getContext()), 0)});

        correctImmediates[ptr].emplace(ptr->getOperand(1), 1);
        correctImmediates[ptr].emplace(ptr->getOperand(3), 3);

        root->replaceAllUsesWith(ptr);
        root->eraseFromParent();
    }
    std::uint16_t getSize() override { return 1; }
};

struct GEPi : public Pattern {
    std::unordered_map<llvm::GetElementPtrInst*, std::uint8_t> byteStoreMap;

    bool matches(llvm::Value* rootVal) override {
        if (auto* root = dyn_cast<llvm::GetElementPtrInst>(rootVal)) {
            return dyn_cast<llvm::ConstantInt>(*root->idx_begin());
        }
        return false;
    }
    void markCovered(llvm::Value* rootVal, std::unordered_set<llvm::Value*>& covered) override {
        byteStoreMap[cast<llvm::GetElementPtrInst>(rootVal)] =
            getBytesFromIntType(cast<llvm::GetElementPtrInst>(rootVal)->getSourceElementType());
        covered.insert(rootVal);
        covered.insert((*cast<llvm::GetElementPtrInst>(rootVal)->idx_begin()));
    }
    void replace(llvm::Module& module, llvm::Value* rootVal, llvm::Value* parent) override {
        auto* root = cast<llvm::GetElementPtrInst>(rootVal);
        assert(root->getNumIndices() == 1);
        llvm::IRBuilder<> builder{root};
        auto* imm =
            llvm::ConstantInt::get(llvm::Type::getInt64Ty(module.getContext()),
                                   cast<llvm::ConstantInt>(*root->idx_begin())->getSExtValue() * byteStoreMap[root]);
        imm->mutateType(llvm::Type::getInt64Ty(module.getContext()));

        auto ptr = builder.CreateCall(getInstruction(module, "LEA64rm", llvm::Type::getInt64Ty(module.getContext()),
                                                     {
                                                         llvm::Type::getInt64Ty(module.getContext()),
                                                         llvm::Type::getInt64Ty(module.getContext()),
                                                         llvm::Type::getInt64Ty(module.getContext()),
                                                         llvm::Type::getInt64Ty(module.getContext()),
                                                     }),
                                      {root->getPointerOperand(),
                                       llvm::ConstantInt::get(llvm::Type::getInt64Ty(module.getContext()), 8),
                                       llvm::ConstantInt::get(llvm::Type::getInt64Ty(module.getContext()), 0), imm});

        correctImmediates[ptr].emplace(ptr->getOperand(1), 1);
        // correctImmediates[ptr].insert(ptr->getOperand(2));
        correctImmediates[ptr].emplace(ptr->getOperand(3), 3);
        root->replaceAllUsesWith(ptr);
        root->eraseFromParent();
    }
    std::uint16_t getSize() override { return 2; }
};

std::vector<std::unique_ptr<Pattern>> fillPatterns() noexcept {
    std::vector<std::unique_ptr<Pattern>> patterns;
    patterns.push_back(std::make_unique<ADD64rr>());
    patterns.push_back(std::make_unique<ADD64ri>());
    patterns.push_back(std::make_unique<MOV64ri>());
    patterns.push_back(std::make_unique<SUB64rr>());
    patterns.push_back(std::make_unique<SUB64ri>());
    patterns.push_back(std::make_unique<XOR64rr>());
    patterns.push_back(std::make_unique<XOR64ri>());
    patterns.push_back(std::make_unique<Compare64rr>());
    patterns.push_back(std::make_unique<Compare64ri>());
    patterns.push_back(std::make_unique<Zext>());
    patterns.push_back(std::make_unique<Loadrm>());
    patterns.push_back(std::make_unique<Storemr>());
    patterns.push_back(std::make_unique<Storemi>());
    patterns.push_back(std::make_unique<AllocaDummy>());
    patterns.push_back(std::make_unique<Br>());
    patterns.push_back(std::make_unique<GEP>());
    patterns.push_back(std::make_unique<GEPi>());
    patterns.push_back(std::make_unique<PtrToIntDummy>());
    patterns.push_back(std::make_unique<IntToPTrDummy>());
    patterns.push_back(std::make_unique<Sext>());
    patterns.push_back(std::make_unique<PhiDummy>());
    patterns.push_back(std::make_unique<BrAndCompRR>());
    patterns.push_back(std::make_unique<CallDummy>());
    patterns.push_back(std::make_unique<BrAndCompRI>());
    patterns.push_back(std::make_unique<MUL64rr>());
    patterns.push_back(std::make_unique<MUL64ri>());
    patterns.push_back(std::make_unique<And64rr>());
    patterns.push_back(std::make_unique<Or64rr>());
    patterns.push_back(std::make_unique<And64ri>());
    patterns.push_back(std::make_unique<Or64ri>());
    patterns.push_back(std::make_unique<SHL64rr>());
    patterns.push_back(std::make_unique<SHL64ri>());
    patterns.push_back(std::make_unique<SAR64rr>());
    patterns.push_back(std::make_unique<SAR64ri>());
    patterns.push_back(std::make_unique<LEA_instead_of_add_mul>());

    std::sort(patterns.begin(), patterns.end(),
              [](const std::unique_ptr<Pattern>& p1, const std::unique_ptr<Pattern>& p2) {
                  return p1->getSize() > p2->getSize();
              });
    return patterns;
}

static std::vector<std::unique_ptr<Pattern>> patterns = fillPatterns();

struct Context {
    std::vector<std::tuple<llvm::Value*, Pattern*, llvm::Value*>> selectionMap;
    // std::unordered_set<llvm::Value*> covered;
    std::unordered_set<llvm::Value*> selected;
};

// TODO: canonicalisation, such that no op has 2 constants

void selectInstruction(llvm::Instruction& instr, Context& context, std::unordered_set<llvm::Value*>& localCovered);

void selectValue(llvm::Value* val, Context& context, llvm::Value* parent,
                 std::unordered_set<llvm::Value*>& localCovered) {
    if (dyn_cast<llvm::Function>(val) || dyn_cast<llvm::BasicBlock>(val)) // in calls / br
        return;
    if (auto* instr = dyn_cast<llvm::Instruction>(val))
        selectInstruction(*instr, context, localCovered);
    else {
        if (dyn_cast<llvm::Argument>(val)) {
            return;
        }
        // else do nothing now ig
    }
}

void selectInstruction(llvm::Instruction& instr, Context& context, std::unordered_set<llvm::Value*>& localCovered) {
    if (context.selected.contains(&instr))
        return; // already selected - no need to select for it
    if (instr.getOpcode() != llvm::Instruction::Ret && !localCovered.contains(&instr)) {
        auto pattern =
            std::find_if(patterns.begin(), patterns.end(),
                         [instr = &instr](const std::unique_ptr<Pattern>& pattern) { return pattern->matches(instr); });
        if (pattern == patterns.end()) {
            instr.print(llvm::errs());
            throw std::runtime_error("could not select instruction");
        }
        context.selected.insert(&instr);
        context.selectionMap.emplace_back(&instr, pattern->get(), nullptr);
        (*pattern)->markCovered(&instr, localCovered);
    }
    if (dyn_cast<llvm::AllocaInst>(&instr))
        return;
    for (auto& operand : instr.operands()) {
        //  llvm::errs() << "looking at operand ";
        // operand->print(llvm::errs());
        // llvm::errs() << "\n";
        selectValue(operand, context, &instr, localCovered);
    }
}

void dfs(std::unordered_set<llvm::Value*>& visited, llvm::Value* curr) {
    if (visited.contains(curr)) {
        return;
    }
    visited.insert(curr);
    if (auto* instr = dyn_cast<llvm::Instruction>(curr)) {
        for (auto& op : instr->operands()) {
            dfs(visited, op);
        }
    }
}

void fixupConstants(llvm::Function& func) {
    std::unordered_map<std::int64_t, llvm::Value*> constants;
    for (auto& bb : func) {
        for (auto& instr : bb) {
            if (dyn_cast<llvm::AllocaInst>(&instr))
                continue;
            for (auto [i, op] : std::views::enumerate(instr.operands())) {

                if (auto* constInt = dyn_cast<llvm::ConstantInt>(op)) {
                    if (correctImmediates[&instr].contains({op, i})) {
                        continue;
                    }
                    if (!constants.contains(constInt->getSExtValue())) {
                        llvm::IRBuilder<> builder{func.getContext()};
                        builder.SetInsertPoint(func.getEntryBlock().getFirstInsertionPt());
                        auto* call = builder.CreateCall(
                            getInstruction(*func.getParent(), "MOV64ri", llvm::Type::getInt64Ty(func.getContext()),
                                           {
                                               llvm::Type::getInt64Ty(func.getContext()),
                                           }),
                            {llvm::ConstantInt::get(llvm::Type::getInt64Ty(func.getContext()), constInt->getSExtValue(),
                                                    true)});
                        constants[constInt->getSExtValue()] = call;
                    }
                    // llvm::errs() << "replacing constant int op  " << i << " :";
                    // constInt->print(llvm::errs());
                    // llvm::errs() << " in instr ";
                    // instr.print(llvm::errs());
                    // llvm::errs() << "\n";
                    instr.setOperand(i, constants[constInt->getSExtValue()]);
                }
            }
        }
    }
}

std::vector<llvm::Instruction*> extractRoots(llvm::Function& func) {
    std::unordered_set<llvm::Value*> visited;
    std::vector<llvm::Instruction*> roots;
    for (auto& bb : llvm::reverse(func)) {
        for (auto& instr : llvm::reverse(bb)) {
            if (visited.contains(&instr)) {
                continue;
            }
            roots.push_back(&instr);
            dfs(visited, &instr);
        }
    }
    return roots;
}

void selectFunction(llvm::Function& func) {
    Context context;
    auto roots = extractRoots(func);
    // llvm::errs() << "selecting func " << func.getName() << "\n";
    for (auto& root : roots) {
        //  llvm::errs() << "starting selecting ...\n";
        std::unordered_set<llvm::Value*> localCovered;
        selectInstruction(*root, context, localCovered);
    }

    for (auto& [val, pattern, parent] : std::views::reverse(context.selectionMap)) {
        // llvm::errs() << "replacing ...";
        // val->print(llvm::errs());
        // llvm::errs() << " \n";
        pattern->replace(*func.getParent(), val, parent);
    }
    fixupConstants(func);

    std::vector<llvm::AllocaInst*> allocas;

    for (auto& instr : func.getEntryBlock()) {
        if (auto* alloca = dyn_cast<llvm::AllocaInst>(&instr)) {
            allocas.push_back(alloca);
        }
    }

    // i64 FRAME_SETUP(numStackArgs, stackFrameSize, stackArg0, stackArg1, ...)
    auto* frame_setup_func = getInstruction(*func.getParent(), "FRAME_SETUP", llvm::Type::getInt64Ty(func.getContext()),
                                            {
                                                llvm::Type::getInt64Ty(func.getContext()),
                                                llvm::Type::getInt64Ty(func.getContext()),
                                            },
                                            true);
    auto* frame_destroy_func =
        getInstruction(*func.getParent(), "FRAME_DESTROY", llvm::Type::getVoidTy(func.getContext()),
                       {
                           llvm::Type::getInt64Ty(func.getContext()),
                       });

    std::uint64_t numArgs = func.arg_size();
    std::vector<llvm::Value*> frame_setup_args = {
        llvm::ConstantInt::get(llvm::Type::getInt64Ty(func.getContext()), allocas.size() * 8),
        llvm::ConstantInt::get(llvm::Type::getInt64Ty(func.getContext()), (numArgs <= 6) ? 0 : numArgs - 6)};
    for (size_t i = 6; i < numArgs; ++i) {
        frame_setup_args.push_back(func.getArg(i));
    }

    llvm::IRBuilder<> builder{&func.getEntryBlock(), func.getEntryBlock().getFirstInsertionPt()};
    auto* framepointer = builder.CreateCall(frame_setup_func, std::move(frame_setup_args));

    // replace all stack arguments with framepointer offset
    for (size_t i = 6; i < numArgs; ++i) {
        if (func.getArg(i)->getNumUses() == 1) { // only the frame-setup func
            continue;
        }
        auto loadedVal =
            builder.CreateCall(getInstruction(*func.getParent(), "MOV64rm", llvm::Type::getInt64Ty(func.getContext()),
                                              {
                                                  llvm::Type::getInt64Ty(func.getContext()),
                                                  llvm::Type::getInt64Ty(func.getContext()),
                                                  llvm::Type::getInt64Ty(func.getContext()),
                                                  llvm::Type::getInt64Ty(func.getContext()),
                                              }),
                               {framepointer, llvm::ConstantInt::get(llvm::Type::getInt64Ty(func.getContext()), 8),
                                llvm::ConstantInt::get(llvm::Type::getInt64Ty(func.getContext()), (i - 6)),
                                llvm::ConstantInt::get(llvm::Type::getInt64Ty(func.getContext()), 16)});
        auto* arg = func.getArg(i);
        arg->replaceAllUsesWith(loadedVal);
        framepointer->setArgOperand(2 + (i - 6), arg); // reset it to the actual arg
    }

    // replace all allocas by fp + offset
    for (auto [i, alloca] : std::views::enumerate(allocas)) {
        builder.SetInsertPoint(alloca);

        auto constOffset =
            builder.CreateCall(getInstruction(*func.getParent(), "MOV64ri", llvm::Type::getInt64Ty(func.getContext()),
                                              {
                                                  llvm::Type::getInt64Ty(func.getContext()),
                                              }),
                               {llvm::ConstantInt::get(llvm::Type::getInt64Ty(func.getContext()), -i - 1, true)});

        auto* ptr =
            builder.CreateCall(getInstruction(*func.getParent(), "LEA64rm", llvm::Type::getInt64Ty(func.getContext()),
                                              {
                                                  llvm::Type::getInt64Ty(func.getContext()),
                                                  llvm::Type::getInt64Ty(func.getContext()),
                                                  llvm::Type::getInt64Ty(func.getContext()),
                                                  llvm::Type::getInt64Ty(func.getContext()),
                                              }),
                               {framepointer, llvm::ConstantInt::get(llvm::Type::getInt64Ty(func.getContext()), 8),
                                constOffset, llvm::ConstantInt::get(llvm::Type::getInt64Ty(func.getContext()), 0)});
        correctImmediates[ptr].emplace(ptr->getOperand(1), 1);
        correctImmediates[ptr].emplace(ptr->getOperand(3), 3);
        alloca->replaceAllUsesWith(ptr);
        alloca->eraseFromParent();
    }

    for (auto& bb : func) {
        for (auto& instr : llvm::make_early_inc_range(bb)) {
            if (instr.getOpcode() == llvm::Instruction::Ret) {
                llvm::IRBuilder<>{&instr}.CreateCall(frame_destroy_func, {framepointer});
            }
        }
    }
}

void cleanUp(llvm::Function& func) {
    constexpr static auto cleanDoubleAndi = [](llvm::Instruction* inst) {
        if (auto* call = dyn_cast<llvm::CallInst>(inst)) {
            if (call->getCalledFunction()->getName() != "AND64ri") {
                return;
            }
            // sometimes we have the case we get sth like
            //  %4 = call i64 @SETcc8r(i64 4)
            //  %5 = call i64 @AND64ri(i64 %4, i64 1)
            //  %6 = call i64 @AND64ri(i64 %5, i64 1)
            if (auto* operandCall = dyn_cast<llvm::CallInst>(call->getArgOperand(0))) {
                if (operandCall->getCalledFunction()->getName() != "AND64ri") {
                    return;
                }
                if (operandCall->getArgOperand(1) == call->getArgOperand(1)) {
                    call->replaceAllUsesWith(operandCall);
                    call->eraseFromParent();
                }
            }
        }
    };
    for (auto& bb : func) {
        for (auto& instr : llvm::make_early_inc_range(llvm::reverse(bb))) {
            cleanDoubleAndi(&instr);
        }
    }
}
} // namespace

void doInstructionSelection(llvm::Module& module) {
    correctImmediates.clear();
    for (auto& func : module) {
        if (!func.isDeclaration()) {
            selectFunction(func);
            cleanUp(func);
        }
    }
    if (llvm::verifyModule(module, &llvm::errs())) {
        // module.print(llvm::errs(), nullptr);

        throw std::runtime_error("Invalid IR!");
    }
}