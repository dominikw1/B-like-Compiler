all:
	cmake --build build -j16
	
test:
	ctest --output-on-failure -j16

configure:
	cmake -S . -B build

clean:
	rm -rf build