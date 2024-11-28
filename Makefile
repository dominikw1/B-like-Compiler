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


CXX_FLAGS := -O3 -std=c++23 -march=native -mtune=native -flto=auto

parser: 
	$(CXX) -c -fPIC $(CXX_FLAGS) src/Parser/Scanner.cpp src/Parser/Parser.cpp src/Parser/AST.cpp	


LLVM_CONFIG := llvm-config
LLVM_FLAGS := $(shell $(LLVM_CONFIG) --cppflags --ldflags --libs)


