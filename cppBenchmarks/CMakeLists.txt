cmake_minimum_required(VERSION 3.10)
project(Benchmarks VERSION 0.0.1
        DESCRIPTION "Microbenchmarks"
        LANGUAGES CXX)   
set(CMAKE_CXX_STANDARD 17)
list(APPEND CMAKE_MODULE_PATH /usr/lib/cmake/benchmark)

find_package(benchmark REQUIRED) 

# Possible flags: -mtune=native, -march=native, -O2 -O3
# Override all the flags
set(CMAKE_CXX_FLAGS_RELEASE "-O2 -Wall -Werror")

# All the binaries use the same compiler commands
link_libraries(benchmark::benchmark pthread)

add_executable(constructorTest constructorTest.cpp)
add_executable(loopTest loopTest.cpp)
add_executable(stringTest stringTest.cpp)