cmake_minimum_required(VERSION 3.28.3)

project(ChocoPy VERSION 1.0)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED YES)
set(CMAKE_CXX_EXTENSIONS NO)
set(CMAKE_CXX_FLAGS_DEBUG "-g -O0")

set(CMAKE_C_COMPILER "/usr/bin/clang-17")
set(CMAKE_CXX_COMPILER "/usr/bin/clang++-17")

add_executable(chocopyc chocopy.cc)
