all:
	cmake --build build -j16 && cd build && ctest --output-on-failure -j16

configure:
	cmake -S . -B build

clean:
	rm -rf build

run:
	./build/src/Compiler
noTest:
	cmake --build build -j16

.PHONY: bc0
bc0: 
	$(CXX) -march=native -mtune=native -flto=auto -O3 -std=c++23 bc0.cc src/Parser/Scanner.cpp src/Parser/Parser.cpp src/Parser/AST.cpp -o bc0


LLVM_CONFIG := llvm-config
LLVM_FLAGS := $(shell $(LLVM_CONFIG) --cppflags --ldflags --libs)

.PHONY: bc1
bc1:
	$(CXX) -O3 -std=c++23 src/main.cpp src/Parser/Scanner.cpp src/Parser/Parser.cpp src/Parser/AST.cpp src/IRGenerator/ValueTracker.cpp src/IRGenerator/SSAGeneration.cpp -o bc1 -I src $(LLVM_FLAGS)
