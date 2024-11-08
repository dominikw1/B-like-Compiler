add_library(IRGenerator SSAGeneration.cpp ValueTracker.cpp)
target_link_libraries(IRGenerator LLVM) 