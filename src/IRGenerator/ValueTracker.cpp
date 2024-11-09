#include "ValueTracker.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

void ValueTracker::writeVariable(std::string_view var, const llvm::BasicBlock* block, llvm::Value* value) {
    currentDef[var][block] = value;
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
    return uniqueVal;
}

llvm::Value* ValueTracker::addPhiOperands(std::string_view var, llvm::PHINode* phi, llvm::BasicBlock* block) {
    for (auto* pred : llvm::predecessors(block)) {
        auto* newV = readVariable(var, pred);
        if (newV->getType() != phi->getType()) {
            if (phi->getNumIncomingValues() == 0) {
                phi->mutateType(newV->getType());
            } else {
                newV->print(llvm::errs());
                phi->print(llvm::errs());
                throw std::runtime_error("Incompatible types");
            }
        }
        phi->addIncoming(newV, pred);
    }
    return reduceTrivialPhi(phi);
}

void ValueTracker::sealBlock(llvm::BasicBlock* block) {
    for (auto var : incompletePhis[block]) {
        addPhiOperands(var.first, llvm::cast<llvm::PHINode>(var.second), block);
    }
    sealed.insert(block);
}

llvm::Value* ValueTracker::readVariableRecursive(std::string_view var, llvm::BasicBlock* block) {
    llvm::Value* val{};
    if (!sealed.contains(block)) {
        // Incomplete CFG
        val = llvm::PHINode::Create(llvm::Type::getInt64Ty(context), 0, "incompletePhi");
        auto* insertPlace = block->getFirstNonPHI();
        if (insertPlace) {
            llvm::cast<llvm::Instruction>(val)->insertBefore(insertPlace);
        } else {
            builder.SetInsertPoint(block);
            builder.Insert(val);
        }
        incompletePhis[block][var] = val;
        //  currBlock->dump();
    } else if (llvm::pred_size(block) == 1) {
        // Optimize the common case of one predecessor : No phi needed
        val = readVariable(var, *llvm::pred_begin(block));
    } else {
        auto* phi = llvm::PHINode::Create(llvm::Type::getInt64Ty(context), llvm::pred_size(block));
        // TODO: check if splitting would not be smarter. Can this approach cause problems?
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

llvm::Value* ValueTracker::readVariable(std::string_view var, llvm::BasicBlock* block) {
    if (currentDef[var].contains(block)) {
        return currentDef[var][block];
    }
    return readVariableRecursive(var, block);
}
