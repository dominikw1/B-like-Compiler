#include "RegisterAllocator.h"
#include <array>
#include <llvm/ADT/DenseMap.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instructions.h>
static constexpr llvm::Function* getInstruction(llvm::Module& module, llvm::StringRef name, llvm::Type* retType,
                                                std::vector<llvm::Type*> args, bool varags = false) {
    auto* calledFunc = module.getFunction(name);
    if (!calledFunc) {
        auto type = llvm::FunctionType::get(retType, args, varags);
        calledFunc = llvm::cast<llvm::Function>(module.getOrInsertFunction(name, type).getCallee());
    }
    return calledFunc;
}

class RegisterAllocator {
    llvm::DenseMap<llvm::Value*, std::uint64_t> stackSlot;
    std::uint64_t currStackSlot = 0;
    llvm::LLVMContext& context;
    llvm::IRBuilder<> builder;
    llvm::IntegerType* i64Ty;

    constexpr static std::uint8_t RETURN_VALUE_REGISTER = 0;
    constexpr static std::uint8_t DEFAULT_OUTPUT_REGISTER = 1;
    constexpr static std::uint8_t STACK_POINTER_REGISTER = 5;
    constexpr static std::uint8_t FRAME_POINTER_REGISTER = 4;
    constexpr static std::uint8_t SPILL_START_REGISTER = 14;
    constexpr static std::uint8_t ZERO_REGISTER = 15;

    std::uint8_t getRegisterForOperand(std::uint8_t operandIndex) {
        switch (operandIndex) {
        case 0:
            return DEFAULT_OUTPUT_REGISTER;
        case 1:
            return 2;
        case 2:
            return 3;
        default:
            throw std::runtime_error("Too many operands!");
        }
    }

    llvm::ConstantInt* getI64(std::int64_t v) { return llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), v); }

  public:
    RegisterAllocator(llvm::Module* module)
        : context{module->getContext()}, i64Ty{llvm::Type::getInt64Ty(context)}, builder{context} {}

  private:
    // Precondition: set builder insert point to correct place
    void loadFromStack(std::uint8_t dest, llvm::Value* val, llvm::Module& module) {
        builder.CreateCall(
            getInstruction(module, "R_MOV64rm", llvm::Type::getVoidTy(context), {i64Ty, i64Ty, i64Ty, i64Ty, i64Ty}),
            {getI64(dest), getI64(SPILL_START_REGISTER), getI64(8), getI64(ZERO_REGISTER), getI64(stackSlot[val] * 8)},"load");
    }

    void spillToStack(llvm::Instruction& instr) {
        // if (auto* call = dyn_cast<llvm::CallInst>(&instr);
        //   (call && call->getFunctionType()->getReturnType()->isVoidTy()) || dyn_cast<llvm::BranchInst>(&instr)) {
        if (dyn_cast<llvm::BranchInst>(&instr)) {
            return; // nothing to spill here
        }
        if (dyn_cast<llvm::ReturnInst>(&instr)) {
            return; // Todo
        }
        instr.print(llvm::errs());
        assert(!stackSlot.contains(&instr));
        builder.SetInsertPoint(instr.getNextNode());
        auto* store = builder.CreateCall(getInstruction(*instr.getModule(), "R_MOV64mr",
                                                        llvm::Type::getVoidTy(instr.getContext()),
                                                        {i64Ty, i64Ty, i64Ty, i64Ty, i64Ty}),
                                         {getI64(SPILL_START_REGISTER), getI64(8), getI64(ZERO_REGISTER),
                                          getI64(currStackSlot * 8), getI64(DEFAULT_OUTPUT_REGISTER)},"spilled");
        stackSlot[&instr] = currStackSlot++;
    }

    constexpr static std::array<std::uint8_t, 6> argumentLocationRegister{7, 6, 2, 1, 8, 9};

    void spillArgsToStack(llvm::Function& func) {
        // Arguments are passed in registers
        // spill all args to stack
        for (size_t i = 0; i < func.arg_size(); ++i) {
            auto* arg = func.getArg(i);
            if (i < 6) {
                // passed in register
                const uint8_t register_num = argumentLocationRegister[i];
                auto* store =
                    builder.CreateCall(getInstruction(*func.getParent(), "R_MOV64mr", llvm::Type::getVoidTy(context),
                                                      {i64Ty, i64Ty, i64Ty, i64Ty, i64Ty}),
                                       {getI64(SPILL_START_REGISTER), getI64(8), getI64(ZERO_REGISTER),
                                        getI64(currStackSlot * 8), getI64(register_num)},
                                       "reg-arg-to-stack");
                stackSlot[arg] = currStackSlot++;
            } else {
                // passed on stack anyway, but let's just copy them to where we can actually use them easily
                builder.CreateCall(getInstruction(*func.getParent(), "R_MOV64rm", llvm::Type::getVoidTy(context),
                                                  {i64Ty, i64Ty, i64Ty, i64Ty, i64Ty}),
                                   {getI64(DEFAULT_OUTPUT_REGISTER), getI64(STACK_POINTER_REGISTER), getI64(8),
                                    getI64(ZERO_REGISTER), getI64(16 + (i - 6) * 8)});
                builder.CreateCall(getInstruction(*func.getParent(), "R_MOV64mr", llvm::Type::getVoidTy(context),
                                                  {i64Ty, i64Ty, i64Ty, i64Ty, i64Ty}),
                                   {getI64(SPILL_START_REGISTER), getI64(8), getI64(ZERO_REGISTER),
                                    getI64(currStackSlot * 8), getI64(DEFAULT_OUTPUT_REGISTER)});
                stackSlot[arg] = currStackSlot++;
            }
        }
    }

    void allocaStackForEveryInst(llvm::Function& func) {
        auto* old_stackSetup = cast<llvm::CallInst>(func.getEntryBlock().getFirstNonPHI());
        std::uint64_t currOffset =
            cast<llvm::ConstantInt>(old_stackSetup->getArgOperand(1))->getZExtValue(); // stackFrameSize
        llvm::SmallVector<llvm::Value*> frameSetupArgs;
        // first take old values and later adjust
        llvm::Value* oldStackSize = old_stackSetup->getArgOperand(0);
        llvm::Value* oldNumStackArgs = old_stackSetup->getArgOperand(1);
        frameSetupArgs.push_back(oldStackSize);
        frameSetupArgs.push_back(getI64(func.arg_size()));
        for (auto& arg : func.args()) {
            frameSetupArgs.push_back(&arg);
        }

        builder.SetInsertPoint(func.getEntryBlock().getFirstInsertionPt());
        auto* newFrameSetup =
            builder.CreateCall(getInstruction(*func.getParent(), "R_FRAME_SETUP", llvm::Type::getVoidTy(context),
                                              std::vector<llvm::Type*>(frameSetupArgs.size(), i64Ty), true),
                               std::move(frameSetupArgs));
        // old_stackSetup->replaceAllUsesWith(newFrameSetup);
        old_stackSetup->removeFromParent();
        builder.SetInsertPoint(newFrameSetup->getNextNode());
        builder.CreateCall(
            getInstruction(*func.getParent(), "R_MOV64ri", llvm::Type::getVoidTy(context), {i64Ty, i64Ty}),
            {getI64(ZERO_REGISTER), getI64(0)},
            "init-zero-register"); // initialise zero register with 0 - not to be touched
                                   // anymore! TODO: only do this if we are in main
        // initialise stack spill start
        auto* spillStart =
            builder.CreateCall(getInstruction(*func.getParent(), "R_LEA64rm", llvm::Type::getVoidTy(context),
                                              {i64Ty, i64Ty, i64Ty, i64Ty, i64Ty}),
                               {getI64(SPILL_START_REGISTER), getI64(STACK_POINTER_REGISTER), getI64(8),
                                getI64(ZERO_REGISTER), getI64(0 /*- final stacksize, to be adjusted later*/)},
                               "init-stack-spill-start");

        spillArgsToStack(func);
        llvm::SmallVector<llvm::CallInst*> frameDestroys;
        for (auto& bb : llvm::make_early_inc_range(func)) {
            for (auto& inst : llvm::make_early_inc_range(bb)) {
                if (auto* call = dyn_cast<llvm::CallInst>(&inst)) {
                    // this already is a register - allocated instr. Mainly for the 2 calls introduced above
                    if (call->getCalledFunction()->getName().starts_with("R_"))
                        continue;
                    if (call->getCalledFunction()->getName().starts_with("FRAME_DESTROY")) {
                        frameDestroys.push_back(call);
                        continue;
                    }
                }
                spillToStack(inst);
            }
        }

        // handle operands loaded from stack
        for (auto& bb : llvm::make_early_inc_range(func)) {
            for (auto& inst : llvm::make_early_inc_range(bb)) {
                if (auto* call = dyn_cast<llvm::CallInst>(&inst)) {
                    // this already is a register - allocated instr
                    if (call->getCalledFunction()->getName().starts_with("R_") ||
                        call->getCalledFunction()->getName().starts_with("FRAME_DESTROY"))
                        continue;
                }
                if (dyn_cast<llvm::ReturnInst>(&inst))
                    continue; // require special handling in next step

                for (std::size_t i = 0; i < inst.getNumOperands(); ++i) {
                    auto* op = inst.getOperand(i);
                    if (!dyn_cast<llvm::Instruction>(op) && !dyn_cast<llvm::Argument>(op))
                        continue;
                    llvm::errs() << "got past check\n";
                    //          llvm::Instruction* instructionResult = cast<llvm::Instruction>(op);
                    //            assert(stackSlot.contains(instructionResult));
                    builder.SetInsertPoint(&inst);
                    loadFromStack(getRegisterForOperand(i), op, *func.getParent());
                    inst.setOperand(i, getI64(getRegisterForOperand(i)));
                }
            }
        }

        // handle returns
        for (auto* frame_destroy : frameDestroys) {
            auto* returnInst = cast<llvm::ReturnInst>(frame_destroy->getNextNode());
            builder.SetInsertPoint(returnInst);
            frame_destroy->eraseFromParent();
            if (dyn_cast<llvm::ConstantInt>(returnInst->getReturnValue())) {
                builder.CreateCall(
                    getInstruction(*func.getParent(), "R_MOV64ri", llvm::Type::getVoidTy(context), {i64Ty, i64Ty}),
                    {getI64(RETURN_VALUE_REGISTER), returnInst->getReturnValue()});
            } else {
                assert(stackSlot.contains(returnInst->getReturnValue()));
                loadFromStack(RETURN_VALUE_REGISTER, returnInst->getReturnValue(), *func.getParent());
            }
            auto* destroy = builder.CreateCall(
                getInstruction(*func.getParent(), "R_FRAME_DESTROY", i64Ty, {llvm::Type::getVoidTy(context)}), {});
            returnInst->setOperand(0, destroy);
        }

        // TODO: update stack size
        old_stackSetup->deleteValue(); // cleanup only afterwards - we still need the references to it before
    }

  public:
    void allocateFunction(llvm::Function& func) { allocaStackForEveryInst(func); }
};

void allocateRegisters(llvm::Module& module) {
    RegisterAllocator allocator{&module};
    for (auto& func : module) {
        if (func.isDeclaration())
            continue;
        allocator.allocateFunction(func);
    }
}
