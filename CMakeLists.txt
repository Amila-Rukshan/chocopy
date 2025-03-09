cmake_minimum_required(VERSION 3.28.3)

project(ChocoPy VERSION 1.0)

find_package(MLIR REQUIRED CONFIG)

message(STATUS "Using MLIRConfig.cmake in: ${MLIR_DIR}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include(AddLLVM)

message(STATUS "LLVM_INCLUDE_DIRS: ${LLVM_INCLUDE_DIRS}")

include_directories(${LLVM_INCLUDE_DIRS})

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED YES)
set(CMAKE_CXX_EXTENSIONS NO)
set(CMAKE_CXX_FLAGS_DEBUG "-g -O0")

set(CMAKE_C_COMPILER "/usr/bin/clang-17")
set(CMAKE_CXX_COMPILER "/usr/bin/clang++-17")

add_executable(chocopyc chocopy.cc)
