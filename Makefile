all:
	cmake -S . -B build && cmake --build build -j16 && cd build && ctest --output-on-failure -j16

clean:
	rm -rf build
