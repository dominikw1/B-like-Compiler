add_library(ASMPrinter ASMPrinter.cpp Instructions.cpp)
target_link_libraries(ASMPrinter LLVM)