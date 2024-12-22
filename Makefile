all:
	cmake --build build -j16
	
test:
	ctest --output-on-failure -j16

configure:
	cmake -S . -B build

clean:
	rm -rf build
	
noTest:
	cmake --build build -j16


CXX_FLAGS := -O1 -std=c++23

parser: 
	$(CXX) -c -fPIC $(CXX_FLAGS) src/Parser/Scanner.cpp src/Parser/Parser.cpp src/Parser/AST.cpp	


LLVM_CONFIG := llvm-config-20
LLVM_FLAGS := $(shell $(LLVM_CONFIG) --cppflags --ldflags --libs)

bc3: parser
	$(CXX) $(CXX_FLAGS) AST.o Parser.o Scanner.o src/main.cpp src/IRGenerator/ValueTracker.cpp src/IRGenerator/SSAGeneration.cpp src/Optimizer/Optimizer.cpp src/InstructionSelector/InstructionSelector.cpp  src/RegisterAllocator/RegisterAllocator.cpp -o bc3 -I src $(LLVM_FLAGS)

bc2:
	make && cp ./build/src/Compiler bc2
