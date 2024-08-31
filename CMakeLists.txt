cmake_minimum_required(VERSION 3.17)
project(Compiler)

add_compile_options(-Wall -Wextra -pedantic)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

enable_testing()

add_subdirectory(src)
add_subdirectory(tests)
