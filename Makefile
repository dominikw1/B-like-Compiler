all:
	cmake --build build -j16 && cd build && ctest --output-on-failure -j16

configure:
	cmake -S . -B build

clean:
	rm -rf build
