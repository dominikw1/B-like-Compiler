include(FetchContent)
FetchContent_Declare(
        googletest
        GIT_REPOSITORY https://github.com/google/googletest.git
        GIT_TAG release-1.11.0
)
FetchContent_MakeAvailable(googletest)
include(GoogleTest)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
add_definitions(-g -Wall -Wextra -pedantic)


include_directories(${PROJECT_SOURCE_DIR}/src/Parser)
include_directories(${PROJECT_SOURCE_DIR}/src/IRGenerator)
add_executable(tests LexerTests.cpp ParserTests.cpp IRGenTests.cpp)
target_link_libraries(tests -lubsan)

target_link_libraries(tests gtest_main gtest Parser IRGenerator)
gtest_discover_tests(tests)
