add_library(IRGenerator CFG.cpp SSAGeneration.cpp)
target_link_libraries(IRGenerator LLVM) 