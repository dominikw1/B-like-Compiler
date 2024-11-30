add_subdirectory(Parser)
add_subdirectory(IRGenerator)
add_subdirectory(InstructionSelector)
add_subdirectory(Optimizer)

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
)