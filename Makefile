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


