project(Benchmarks VERSION 0.0.1
        DESCRIPTION "Demonstrations of C++ language performance"
        LANGUAGES CXX)   
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_EXTENSIONS OFF)

# Possible flags: -mtune=native, -march=native, -O2 -O3
# Override all the flags
set(CMAKE_CXX_FLAGS_RELEASE "-O2 -Wall -Werror")

# All the binaries use the same compiler commands
include_directories(thirdParty/benchmark/include)
link_directories(thirdParty/benchmark/build/src)
link_libraries(benchmark pthread)

add_executable(constructorTest constructorTest.cpp)
add_executable(loopTest loopTest.cpp)
add_executable(stringTest stringTest.cpp)