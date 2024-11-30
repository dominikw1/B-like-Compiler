#include "Optimizer.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"

namespace {
void removeNoOps(llvm::Module* module) {
    for (auto& func : *module) {
        for (auto& bb : func) {
            for (auto& instr : llvm::make_early_inc_range(bb)) {
                if (auto* gep = dyn_cast<llvm::GetElementPtrInst>(&instr)) {
                    if (std::all_of(gep->idx_begin(), gep->idx_end(), [](auto& idx) {
                            if (auto* constInt = dyn_cast<llvm::ConstantInt>(&idx)) {
                                return constInt->getSExtValue() == 0;
                            }
                            return false;
                        })) {
                        gep->replaceAllUsesWith(gep->getPointerOperand());
                        gep->removeFromParent();
                    }
                }
            }
        }
    }
}
} // namespace

void optimize(llvm::Module* module) { removeNoOps(module); }