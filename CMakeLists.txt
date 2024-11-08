cmake_minimum_required(VERSION 3.19)
project(Compiler)

add_compile_options(-g)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

find_package(LLVM 19.1.3 REQUIRED CONFIG)
include(AddLLVM)

add_definitions(${LLVM_DEFINITIONS})
include_directories(${LLVM_INCLUDE_DIRS})
include_directories(${PROJECT_SOURCE_DIR}/src)
link_directories(${LLVM_LIBRARY_DIRS})

enable_testing()

add_subdirectory(src)
add_subdirectory(tests)
