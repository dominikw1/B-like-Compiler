#include "RegisterAllocator.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include <array>
#include <llvm/ADT/DenseMap.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instructions.h>
#include <string_view>

static constexpr llvm::Function* getInstruction(llvm::Module& module, llvm::StringRef name, llvm::Type* retType,
                                                std::vector<llvm::Type*> args, bool varags = false) {
    auto* calledFunc = module.getFunction(name);
    if (!calledFunc) {
        auto type = llvm::FunctionType::get(retType, args, varags);
        calledFunc = llvm::cast<llvm::Function>(module.getOrInsertFunction(name, type).getCallee());
    }
    return calledFunc;
}

using namespace std::literals;

class RegisterAllocator {

    llvm::DenseMap<llvm::Value*, std::uint64_t> stackSlot;
    std::uint64_t currStackSlot = 0;
    llvm::LLVMContext& context;
    llvm::IRBuilder<> builder;
    llvm::IntegerType* i64Ty;
    const llvm::DenseSet<llvm::StringRef>& normalFunctions;

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
            throw std::runtime_error("Too many operands!" + std::to_string(operandIndex));
        }
    }

    llvm::ConstantInt* getI64(std::int64_t v) { return llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), v); }

  public:
    RegisterAllocator(llvm::Module* module, const llvm::DenseSet<llvm::StringRef>& normalFuncs)
        : context{module->getContext()}, i64Ty{llvm::Type::getInt64Ty(context)}, builder{context},
          normalFunctions{normalFuncs} {}

  private:
    // Precondition: set builder insert point to correct place
    void loadFromStack(std::uint8_t dest, llvm::Value* val, llvm::Module& module) {
        builder.CreateCall(
            getInstruction(module, "R_MOV64rm", llvm::Type::getVoidTy(context), {i64Ty, i64Ty, i64Ty, i64Ty, i64Ty}),
            {getI64(dest), getI64(SPILL_START_REGISTER), getI64(8), getI64(ZERO_REGISTER), getI64(stackSlot[val] * 8)});
    }

    void spillToStackHere(std::uint8_t registerNum, std::uint64_t slot, llvm::Module& module) {
        auto* spill = builder.CreateCall(
            getInstruction(module, "R_MOV64mr", llvm::Type::getVoidTy(context), {i64Ty, i64Ty, i64Ty, i64Ty, i64Ty}),
            {getI64(SPILL_START_REGISTER), getI64(8), getI64(ZERO_REGISTER), getI64(slot * 8), getI64(registerNum)});
        llvm::errs() << "created spill ";
        spill->print(llvm::errs());
        llvm::errs() << "\n";
    }

    void spillToStackAtStackPointerOffset(std::uint8_t registerNum, std::uint64_t stackPointerOffset,
                                          llvm::Module& module) {
        auto* spill = builder.CreateCall(
            getInstruction(module, "R_MOV64mr", llvm::Type::getVoidTy(context), {i64Ty, i64Ty, i64Ty, i64Ty, i64Ty}),
            {getI64(FRAME_POINTER_REGISTER), getI64(8), getI64(ZERO_REGISTER), getI64(stackPointerOffset),
             getI64(registerNum)});
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
        if (dyn_cast<llvm::PHINode>(&instr)) {
            stackSlot[&instr] = currStackSlot++;
            return; // actual storing to be done later, is more complex
        }
        assert(!stackSlot.contains(&instr));
        builder.SetInsertPoint(instr.getNextNode());
        auto* store = builder.CreateCall(getInstruction(*instr.getModule(), "R_MOV64mr",
                                                        llvm::Type::getVoidTy(instr.getContext()),
                                                        {i64Ty, i64Ty, i64Ty, i64Ty, i64Ty}),
                                         {getI64(SPILL_START_REGISTER), getI64(8), getI64(ZERO_REGISTER),
                                          getI64(currStackSlot * 8), getI64(DEFAULT_OUTPUT_REGISTER)});
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
                                        getI64(currStackSlot * 8), getI64(register_num)});
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

    struct FrameState {
        llvm::CallInst* oldSetupCall;
        llvm::CallInst* spillInit;
    };

    FrameState initState(llvm::Function& func) {
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
                                              std::vector<llvm::Type*>(2, i64Ty), true),
                               std::move(frameSetupArgs));
        // old_stackSetup->replaceAllUsesWith(newFrameSetup);
        old_stackSetup->removeFromParent();
        builder.SetInsertPoint(newFrameSetup->getNextNode());
        builder.CreateCall(
            getInstruction(*func.getParent(), "R_MOV64ri", llvm::Type::getVoidTy(context), {i64Ty, i64Ty}),
            {getI64(ZERO_REGISTER), getI64(0)}); // initialise zero register with 0 - not to be touched
                                                 // anymore! TODO: only do this if we are in main
        // initialise stack spill start
        auto* spillStart =
            builder.CreateCall(getInstruction(*func.getParent(), "R_LEA64rm", llvm::Type::getVoidTy(context),
                                              {i64Ty, i64Ty, i64Ty, i64Ty, i64Ty}),
                               {getI64(SPILL_START_REGISTER), getI64(STACK_POINTER_REGISTER), getI64(8),
                                getI64(ZERO_REGISTER), getI64(0 /*- final stacksize, to be adjusted later*/)});

        return {old_stackSetup, spillStart};
    }

    auto allocaStackForEveryInst(llvm::Function& func) {
        llvm::SmallVector<llvm::CallInst*> frameDestroys;
        for (auto& bb : llvm::make_early_inc_range(func)) {
            for (auto& inst : llvm::make_early_inc_range(bb)) {
                if (auto* call = dyn_cast<llvm::CallInst>(&inst)) {
                    // this already is a register - allocated instr. Mainly for the 2 calls introduced above
                    if (call->getCalledFunction()->getName().starts_with("R_"))
                        continue;
                    if (call->getCalledFunction()->getName().starts_with("Jcc"))
                        continue;
                    if (call->getCalledFunction()->getName().starts_with("FRAME_DESTROY")) {
                        frameDestroys.push_back(call);
                        continue;
                    }
                    if (call->getCalledFunction()->getReturnType()->isVoidTy()) {
                        continue;
                    }
                }
                spillToStack(inst);
            }
        }
        return frameDestroys;
    }

    void handlePhis(llvm::Function& func) {
        //  func.print(llvm::errs());
        for (auto& bb : func) {
            for (auto* pred : llvm::predecessors(&bb)) {
                if (cast<llvm::BranchInst>(&*pred->rbegin())->isConditional()) {
                    // skip over cmp and jcc which have to be there
                    auto* insertPt = pred->rbegin()->getPrevNode()->getPrevNode();
                    assert(dyn_cast<llvm::CallInst>(insertPt));
                    builder.SetInsertPoint(insertPt);
                } else {
                    builder.SetInsertPoint(cast<llvm::Instruction>(&*pred->rbegin()));
                }
                // TODO: if complex cycle
                // else
                for (auto& phi : bb.phis()) {
                    llvm::errs() << "handling " << phi.getName() << " for block " << pred->getName() << "\n";
                    //            phi.print(llvm::errs());
                    //          llvm::errs()<<"stack slot:" << stackSlot[&phi]<<"\n";
                    assert(stackSlot.contains(&phi));
                    loadFromStack(DEFAULT_OUTPUT_REGISTER, phi.getIncomingValueForBlock(pred), *func.getParent());
                    spillToStackHere(DEFAULT_OUTPUT_REGISTER, stackSlot[&phi], *func.getParent());
                    //        llvm::errs() << "\n after handling:\n";
                    //      func.print(llvm::errs());
                    //    llvm::errs() << "\n";
                }
            }
        }
    }

    void loadSpilledOperands(llvm::Function& func) {
        // handle operands loaded from stack
        for (auto& bb : func) {
            for (auto& inst : llvm::make_early_inc_range(bb)) {
                if (dyn_cast<llvm::PHINode>(&inst)) {
                    // to be handled later
                    continue;
                }
                if (auto* call = dyn_cast<llvm::CallInst>(&inst)) {
                    // this already is a register - allocated instr
                    if (call->getCalledFunction()->getName().starts_with("R_") ||
                        call->getCalledFunction()->getName().starts_with("FRAME_DESTROY") ||
                        normalFunctions.contains(call->getCalledFunction()->getName())) {
                        llvm::errs() << "skipping func " << call->getCalledFunction()->getName() << "\n";
                        continue;
                    }
                }
                if (dyn_cast<llvm::ReturnInst>(&inst) || dyn_cast<llvm::BranchInst>(&inst))
                    continue; // require special handling in next step

                llvm::errs() << "loading ops for \n";
                inst.print(llvm::errs());
                std::size_t skipped = 0;
                for (std::size_t i = 0; i < inst.getNumOperands(); ++i) {
                    auto* op = inst.getOperand(i);
                    if (!dyn_cast<llvm::Instruction>(op) && !dyn_cast<llvm::Argument>(op)) {
                        ++skipped;
                        continue;
                    }
                    if (auto* call = dyn_cast<llvm::CallInst>(op);
                        call && call->getCalledFunction()->getName().starts_with("FRAME_SETUP")) {
                        inst.setOperand(i, getI64(STACK_POINTER_REGISTER));
                        ++skipped;
                        continue;
                    }

                    // llvm::errs() << "got past check\n";
                    //           llvm::Instruction* instructionResult = cast<llvm::Instruction>(op);
                    //             assert(stackSlot.contains(instructionResult));
                    builder.SetInsertPoint(&inst);
                    loadFromStack(getRegisterForOperand(i - skipped), op, *func.getParent());
                    inst.setOperand(i, getI64(getRegisterForOperand(i - skipped)));
                }
            }
        }
    }

    void handleReturns(llvm::Function& func, llvm::SmallVector<llvm::CallInst*>& frameDestroys) {
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
                llvm::errs() << "loading for return: " << stackSlot[returnInst->getReturnValue()] * 8 << "\n";
                returnInst->getReturnValue()->print(llvm::errs());
                assert(stackSlot.contains(returnInst->getReturnValue()));
                loadFromStack(RETURN_VALUE_REGISTER, returnInst->getReturnValue(), *func.getParent());
            }
            auto* destroy = builder.CreateCall(getInstruction(*func.getParent(), "R_FRAME_DESTROY", i64Ty, {}), {});
            returnInst->setOperand(0, destroy);
        }
    }

    void updateStackSize(llvm::Function& func, llvm::CallInst* spillInit) {
        std::uint64_t slots = stackSlot.size();
        auto* stack_alloc = cast<llvm::CallInst>(func.getEntryBlock().getFirstInsertionPt());
        std::uint64_t currVal = dyn_cast<llvm::ConstantInt>(stack_alloc->getOperand(0))->getZExtValue();
        std::uint64_t supposedStackSize = slots * 8 + currVal;
        stack_alloc->setOperand(0, getI64(supposedStackSize + 16));
        spillInit->setOperand(4, getI64(-supposedStackSize - 16));
        // the 16 offset might be unnecessary idk it segfaults if i dont do it because we overwrite fp
    }

    void transformCall(llvm::CallInst* oldCall) {
        oldCall->print(llvm::errs());
        llvm::errs() << "\n";
        builder.SetInsertPoint(oldCall);
        if (oldCall->arg_size() > 6) {
            builder.CreateCall(
                getInstruction(*oldCall->getModule(), "R_ADJSTACKDOWN", llvm::Type::getVoidTy(context), {i64Ty}),
                {getI64(oldCall->arg_size() - 6)});
        }
        for (size_t i = 0; i < oldCall->arg_size(); ++i) {
            if (i < argumentLocationRegister.size()) {
                if (dyn_cast<llvm::ConstantInt>(oldCall->getArgOperand(i))) {
                    llvm::errs() << "is imm\n";
                    oldCall->getArgOperand(i)->print(llvm::errs());
                    builder.CreateCall(getInstruction(*oldCall->getModule(), "R_MOV64ri",
                                                      llvm::Type::getVoidTy(context), {i64Ty, i64Ty}),
                                       {getI64(argumentLocationRegister[i]), oldCall->getArgOperand(i)});
                } else {
                    loadFromStack(argumentLocationRegister[i], oldCall->getArgOperand(i), *oldCall->getModule());
                }
            } else {
                if (dyn_cast<llvm::ConstantInt>(oldCall->getArgOperand(i))) {
                    builder.CreateCall(getInstruction(*oldCall->getModule(), "R_MOV64ri",
                                                      llvm::Type::getVoidTy(context), {i64Ty, i64Ty}),
                                       {getI64(DEFAULT_OUTPUT_REGISTER), oldCall->getArgOperand(i)});
                } else {
                    loadFromStack(DEFAULT_OUTPUT_REGISTER, oldCall->getArgOperand(i), *oldCall->getModule());
                }
                spillToStackAtStackPointerOffset(DEFAULT_OUTPUT_REGISTER, (i - 6) * 8, *oldCall->getModule());
            }
        }

        auto* func = getInstruction(*oldCall->getModule(), "R_CALL", llvm::Type::getVoidTy(context),
                                    {llvm::PointerType::get(context,0)});
        builder.CreateCall(func, oldCall->getCalledFunction());
        if (oldCall->arg_size() > 6) {
            builder.CreateCall(
                getInstruction(*oldCall->getModule(), "R_ADJSTACKUP", llvm::Type::getVoidTy(context), {i64Ty}),
                {getI64(oldCall->arg_size() - 6)});
        }
        spillToStackHere(RETURN_VALUE_REGISTER, stackSlot[oldCall], *oldCall->getModule());
    }

    constexpr static std::array<std::string_view, 23> destShiftInstructions_PreReg{
        "MOV64rr",    "MOV64ri",    "MOV32rr",    "MOV32ri",    "MOV64rm",    "MOV32rm",    "MOVZXB32rr", "MOVZXB32rm",
        "MOVZXW32rr", "MOVZXW32rm", "MOVSXB32rr", "MOVSXB32rm", "MOVSXB64rr", "MOVSXB64rm", "MOVSXW32rr", "MOVSXW32rm",
        "MOVSXW64rr", "MOVSXW64rm", "MOVSXWD4rr", "MOVSXWD4rm", "LEA64rm",    "IMUL64rri",  "SETcc8r"};

    constexpr static std::array<std::string_view, 23> destShiftInstructions{
        "R_MOV64rr",    "R_MOV64ri",    "R_MOV32rr",    "R_MOV32ri",    "R_MOV64rm",    "R_MOV32rm",
        "R_MOVZXB32rr", "R_MOVZXB32rm", "R_MOVZXW32rr", "R_MOVZXW32rm", "R_MOVSXB32rr", "R_MOVSXB32rm",
        "R_MOVSXB64rr", "R_MOVSXB64rm", "R_MOVSXW32rr", "R_MOVSXW32rm", "R_MOVSXW64rr", "R_MOVSXW64rm",
        "R_MOVSXWD4rr", "R_MOVSXWD4rm", "R_LEA64rm",    "R_IMUL64rri",  "R_SETcc8r"};

    constexpr static std::array<std::string_view, 21> firstOpDestInstructions_PreReg{
        "SUB64rr", "SUB64ri", "ADD64rr", "ADD64ri", "IMUL64rr", "AND64rr", "AND64ri", "OR64rr",  "OR64ri",
        "XOR64rr", "XOR64ri", "SHL64rr", "SHL64ri", "SHR64rr",  "SHR64ri", "SAR64rr", "SAR64ri", "SETcc8r"};

    constexpr static std::array<std::string_view, 21> firstOpDestInstructions{
        "R_SUB64rr", "R_SUB64ri", "R_ADD64rr", "R_ADD64ri", "R_IMUL64rr", "R_AND64rr",
        "R_AND64ri", "R_OR64rr",  "R_OR64ri",  "R_XOR64rr", "R_XOR64ri",  "R_SHL64rr",
        "R_SHL64ri", "R_SHR64rr", "R_SHR64ri", "R_SAR64rr", "R_SAR64ri",  "R_SETcc8r"};

    void transformInstructions(llvm::Function& func) {
        for (auto& bb : func) {
            for (auto& instr : bb) {
                if (auto* call = dyn_cast<llvm::CallInst>(&instr)) {
                    if (call->getCalledFunction()->getName().starts_with("R_") ||
                        call->getCalledFunction()->getName().starts_with("Jcc")) {
                        // llvm::errs() << "skipping transform of " << call->getCalledFunction()->getName() << "\n";
                        continue;
                    }
                    if (!call->getCalledFunction()->getFunctionType()->getReturnType()->isVoidTy()) {
                        // not just simple rename
                        if (auto it =
                                std::find(destShiftInstructions_PreReg.begin(), destShiftInstructions_PreReg.end(),
                                          std::string_view{call->getCalledFunction()->getName()});
                            it != destShiftInstructions_PreReg.end()) {
                            llvm::SmallVector<llvm::Value*> args;
                            args.push_back(getI64(DEFAULT_OUTPUT_REGISTER));
                            for (auto& old_arg : call->args()) {
                                args.push_back(old_arg);
                            }

                            auto* calledFunc = getInstruction(
                                *func.getParent(), destShiftInstructions[it - destShiftInstructions_PreReg.begin()],
                                llvm::Type::getVoidTy(context), std::vector<llvm::Type*>(call->arg_size() + 1, i64Ty));
                            builder.SetInsertPoint(call);
                            builder.CreateCall(calledFunc, args);
                        } else if (auto it = std::find(firstOpDestInstructions_PreReg.begin(),
                                                       firstOpDestInstructions_PreReg.end(),
                                                       std::string_view{call->getCalledFunction()->getName()});
                                   it != firstOpDestInstructions_PreReg.end()) {
                            // first register is 1 anyway so nothing to do except rename??
                            llvm::SmallVector<llvm::Value*> args;
                            for (auto& old_arg : call->args()) {
                                args.push_back(old_arg);
                            }
                            auto* calledFunc = getInstruction(
                                *func.getParent(), firstOpDestInstructions[it - firstOpDestInstructions_PreReg.begin()],
                                llvm::Type::getVoidTy(context), std::vector<llvm::Type*>(call->arg_size(), i64Ty));
                            builder.SetInsertPoint(call);
                            builder.CreateCall(calledFunc, args);
                        } else {
                            // normal call
                            transformCall(call);
                        }
                    } else {
                        call->print(llvm::errs());
                        // is void
                        std::string_view newName =
                            llvm::StringSwitch<std::string_view>(call->getCalledFunction()->getName())
                                .Case("CMP64rr", "R_CMP64rr"sv)
                                .Case("CMP64ri", "R_CMP64ri"sv)
                                .Case("MOV64mr", "R_MOV64mr"sv)
                                .Case("MOV64mi", "R_MOV64mi"sv)
                                .Case("MOV32mr", "R_MOV32mr"sv)
                                .Case("MOV32mi", "R_MOV32mi"sv)
                                .Case("MOV16mr", "R_MOV16mr"sv)
                                .Case("MOV16mi", "R_MOV16mi"sv)
                                .Case("MOV8mr", "R_MOV8mr"sv)
                                .Case("MOV8mi", "R_MOV8mi"sv);
                        llvm::SmallVector<llvm::Value*> args;
                        for (auto& old_arg : call->args()) {
                            args.push_back(old_arg);
                            llvm::errs() << "arg: ";
                            old_arg->print(llvm::errs());
                            llvm::errs() << "\n";
                        }

                        auto* calledFunc = getInstruction(*func.getParent(), newName, llvm::Type::getVoidTy(context),
                                                          std::vector<llvm::Type*>(call->arg_size(), i64Ty));
                        builder.SetInsertPoint(call);
                        builder.CreateCall(calledFunc, args);
                    }
                }
            }
        }

        func.print(llvm::errs());

        for (auto& bb : func) {
            for (auto& instr : llvm::make_early_inc_range(bb)) {
                if (auto* call = dyn_cast<llvm::CallInst>(&instr)) {
                    if (!call->getCalledFunction()->getName().starts_with("R_") &&
                        !call->getCalledFunction()->getName().starts_with("Jcc")) {
                        call->eraseFromParent();
                        continue;
                    }
                }
                if (dyn_cast<llvm::PHINode>(&instr)) {
                    instr.replaceAllUsesWith(llvm::UndefValue::get(instr.getType()));
                    instr.eraseFromParent();
                }
            }
        }
    }

  public:
    void allocateFunction(llvm::Function& func) {
        llvm::SplitAllCriticalEdges(func);
        FrameState frameState = initState(func);
        spillArgsToStack(func);
        auto frameDestroys = allocaStackForEveryInst(func);
        llvm::errs() << "\n";
        for (auto& [val, slot] : stackSlot) {
            val->print(llvm::errs());
            llvm::errs() << " -> " << slot * 8 << "\n";
        }
        loadSpilledOperands(func);
        handleReturns(func, frameDestroys);
        llvm::errs() << "handle phis\n";
        handlePhis(func);
        llvm::errs() << "handled phis\n";
        transformInstructions(func);
        llvm::errs() << "transformed\n";
        updateStackSize(func, frameState.spillInit);
        llvm::errs() << "updated\n";
        frameState.oldSetupCall->deleteValue(); // cleanup only afterwards - we still need the references to it before
        stackSlot.clear();
        currStackSlot = 0;
    }
};

void allocateRegisters(llvm::Module& module, const llvm::DenseSet<llvm::StringRef>& normalFunctions) {
    RegisterAllocator allocator{&module, normalFunctions};
    for (auto& func : module) {
        if (func.isDeclaration())
            continue;
        allocator.allocateFunction(func);
    }
}
