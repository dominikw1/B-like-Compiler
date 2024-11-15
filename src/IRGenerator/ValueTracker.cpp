#include "ValueTracker.h"
#include "llvm/IR/Type.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

void ValueTracker::writeVariable(std::string_view var, const llvm::BasicBlock* block, llvm::Value* value) {
    currentDef[var][block] = value;
    valueToLocation[value].emplace_back(block, var);
}

llvm::Value* ValueTracker::reduceTrivialPhi(llvm::PHINode* phi) {
    llvm::Value* uniqueVal = phi->hasConstantValue();
    if (!uniqueVal) {
        return phi;
    }

    std::vector<llvm::Value*> users;
    for (auto* user : phi->users()) {
        if (user != phi)
            users.push_back(user);
    }
    auto phiIt = phi->getIterator();
    llvm::ReplaceInstWithValue(phiIt, uniqueVal);
    for (auto* user : users) {
        if (user && llvm::isa<llvm::PHINode>(user)) {
            reduceTrivialPhi(llvm::cast<llvm::PHINode>(user));
        }
    }

    for (const auto& [block, name] : valueToLocation[phi]) {
        currentDef[name][block] = uniqueVal;
    }
    valueToLocation.erase(phi);

    return uniqueVal;
}

llvm::Value* ValueTracker::castToType(llvm::Value* val, llvm::Type* type) {
    assert(type->isIntegerTy() || type->isPointerTy());
    assert(val->getType()->isIntegerTy() || val->getType()->isPointerTy());
    assert((type->isIntegerTy() && val->getType()->isPointerTy()) ||
           (type->isPointerTy() && val->getType()->isIntegerTy()));

    if (dyn_cast<llvm::PHINode>(val)) {
        builder.SetInsertPoint(cast<llvm::Instruction>(val)->getParent()->getFirstInsertionPt());
    } else {
        if (auto* param = llvm::dyn_cast<llvm::Argument>(val)) {
            builder.SetInsertPoint(param->getParent()->getEntryBlock().getFirstInsertionPt());
        } else {
            builder.SetInsertPoint(cast<llvm::Instruction>(val)->getNextNode());
        }
    }
    if (type->isIntegerTy()) {
        return builder.CreatePtrToInt(val, type);
    } else {
        return builder.CreateIntToPtr(val, type);
    }
}

llvm::Value* ValueTracker::addPhiOperands(std::string_view var, llvm::PHINode* phi, llvm::BasicBlock* block) {
    for (auto* pred : llvm::predecessors(block)) {
        auto* newV = readVariable(var, phi->getType()->isPointerTy() /*?*/, pred);
        if (newV->getType() != phi->getType()) {
            newV = castToType(newV, phi->getType());
        }
        phi->addIncoming(newV, pred);
    }
    return reduceTrivialPhi(phi);
}

void ValueTracker::sealBlock(llvm::BasicBlock* block) {
    for (auto var : incompletePhis[block]) {
        llvm::PHINode* phi = llvm::cast<llvm::PHINode>(var.second);
        addPhiOperands(var.first, phi, block);
    }
    sealed.insert(block);
}

llvm::Value* ValueTracker::readVariableRecursive(std::string_view var, bool isAlloca, llvm::BasicBlock* block) {
    llvm::Value* val{};
    if (!sealed.contains(block)) {
        // Incomplete CFG
        val = llvm::PHINode::Create(isAlloca ? static_cast<llvm::Type*>(llvm::PointerType::get(context, 0))
                                             : llvm::Type::getInt64Ty(context),
                                    0, "incompletePhi");
        auto* insertPlace = block->getFirstNonPHI();
        if (insertPlace) {
            llvm::cast<llvm::Instruction>(val)->insertBefore(insertPlace);
        } else {
            builder.SetInsertPoint(block);
            builder.Insert(val);
        }
        incompletePhis[block][var] = val;
    } else if (llvm::pred_size(block) == 1) {
        // Optimize the common case of one predecessor : No phi needed
        val = readVariable(var, isAlloca, *llvm::pred_begin(block));
    } else {
        auto* phi = llvm::PHINode::Create(isAlloca ? static_cast<llvm::Type*>(llvm::PointerType::get(context, 0))
                                                   : llvm::Type::getInt64Ty(context),
                                          llvm::pred_size(block));
        auto* insertPlace = block->getFirstNonPHI();
        if (insertPlace) {
            phi->insertBefore(insertPlace);
        } else {
            builder.SetInsertPoint(block);
            builder.Insert(phi);
        }
        writeVariable(var, block, phi);
        val = addPhiOperands(var, phi, block);
    }
    writeVariable(var, block, val);
    return val;
}

llvm::Value* ValueTracker::readVariable(std::string_view var, bool isAlloca, llvm::BasicBlock* block) {
    if (currentDef[var].contains(block)) {
        return currentDef[var][block];
    }
    return readVariableRecursive(var, isAlloca, block);
}
