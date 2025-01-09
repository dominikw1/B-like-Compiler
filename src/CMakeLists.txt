add_subdirectory(Parser)
add_subdirectory(IRGenerator)
add_subdirectory(Optimizer)
add_subdirectory(InstructionSelector)
add_subdirectory(RegisterAllocator)
add_subdirectory(AssemblyPrinter)

add_executable(
    Compiler
    main.cpp
)

target_link_libraries(
    Compiler
    Parser
    IRGenerator
    Optimizer
    InstructionSelector
    RegisterAllocator
    ASMPrinter
)
