cmake_minimum_required(VERSION 3.28.3)

project(ChocoPy VERSION 1.0 LANGUAGES C CXX)

# LLVM 17 from the usual install locations: Homebrew (Apple silicon and
# Intel) and apt.llvm.org/Ubuntu. Set LLVM_DIR to use another install.
find_package(LLVM 17 REQUIRED CONFIG
  HINTS
  /opt/homebrew/opt/llvm@17/lib/cmake/llvm
  /usr/local/opt/llvm@17/lib/cmake/llvm
  /usr/lib/llvm-17/lib/cmake/llvm)
find_package(LLD REQUIRED CONFIG HINTS "${LLVM_LIBRARY_DIR}/cmake/lld")

message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

list(APPEND CMAKE_MODULE_PATH "${LLVM_CMAKE_DIR}")

include(AddLLVM)
include(HandleLLVMOptions)

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

# compile_commands.json for clangd and other editor tooling
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED YES)
set(CMAKE_CXX_EXTENSIONS NO)
set(CMAKE_CXX_FLAGS_DEBUG "-g -O0")

include_directories(include/)

# Minimum macOS version for the runtime library and generated programs
set(CHOCOPY_MACOS_MIN_VERSION "11.0" CACHE STRING "Minimum macOS version of compiled ChocoPy programs")

# Runtime library linked into every compiled ChocoPy program
add_subdirectory(std)

# Platform details the embedded linker needs, detected at configure time
if(APPLE)
  # Programs link against a libSystem stub shipped with the compiler, so no
  # macOS SDK is needed at link time.
  set(CHOCOPY_PLATFORM_DEFINITIONS
    CHOCOPY_MACOS_LIBSYSTEM_STUB="${CMAKE_SOURCE_DIR}/std/libSystem.tbd"
    CHOCOPY_MACOS_MIN_VERSION="${CHOCOPY_MACOS_MIN_VERSION}")
elseif(CMAKE_SYSTEM_NAME STREQUAL "Linux")
  execute_process(COMMAND ${CMAKE_C_COMPILER} -print-file-name=Scrt1.o
    OUTPUT_VARIABLE _scrt1 OUTPUT_STRIP_TRAILING_WHITESPACE)
  execute_process(COMMAND ${CMAKE_C_COMPILER} -print-file-name=crtbeginS.o
    OUTPUT_VARIABLE _crtbegin OUTPUT_STRIP_TRAILING_WHITESPACE)
  get_filename_component(_crt_dir "${_scrt1}" DIRECTORY)
  get_filename_component(_gcc_dir "${_crtbegin}" DIRECTORY)
  set(CHOCOPY_LINUX_CRT_DIR "${_crt_dir}" CACHE PATH "Directory with Scrt1.o, crti.o, crtn.o and libc")
  set(CHOCOPY_LINUX_GCC_DIR "${_gcc_dir}" CACHE PATH "Directory with crtbeginS.o and crtendS.o")
  if(NOT EXISTS "${CHOCOPY_LINUX_CRT_DIR}/Scrt1.o")
    message(FATAL_ERROR "Could not find Scrt1.o; install libc6-dev or set CHOCOPY_LINUX_CRT_DIR")
  endif()

  if(CMAKE_SYSTEM_PROCESSOR MATCHES "aarch64|arm64")
    set(_dynamic_linker "/lib/ld-linux-aarch64.so.1")
  else()
    set(_dynamic_linker "/lib64/ld-linux-x86-64.so.2")
  endif()
  set(CHOCOPY_LINUX_DYNAMIC_LINKER "${_dynamic_linker}" CACHE FILEPATH "Dynamic linker for generated programs")

  set(CHOCOPY_PLATFORM_DEFINITIONS
    CHOCOPY_LINUX_CRT_DIR="${CHOCOPY_LINUX_CRT_DIR}"
    CHOCOPY_LINUX_GCC_DIR="${CHOCOPY_LINUX_GCC_DIR}"
    CHOCOPY_LINUX_DYNAMIC_LINKER="${CHOCOPY_LINUX_DYNAMIC_LINKER}")
endif()

# Front end, LLVM code generation and the embedded linker
add_library(ChocoPyCompiler STATIC
  src/Backend.cpp
  src/CodeGen.cpp
  src/SemanticCheck.cpp)

target_compile_definitions(ChocoPyCompiler
  PRIVATE
  CHOCOPY_RUNTIME_LIB="$<TARGET_FILE:ChocopySupport>"
  ${CHOCOPY_PLATFORM_DEFINITIONS})

target_include_directories(ChocoPyCompiler PUBLIC ${LLD_INCLUDE_DIRS})

if(LLVM_LINK_LLVM_DYLIB)
  set(_llvm_use_shared USE_SHARED)
endif()
llvm_config(ChocoPyCompiler ${_llvm_use_shared}
  native Core Support TargetParser MC Target Passes)

target_link_libraries(ChocoPyCompiler
  PUBLIC
  lldCommon
  lldELF
  lldMachO)

add_dependencies(ChocoPyCompiler ChocopySupport)

add_executable(chocopyc ChocoPy.cpp)

target_link_libraries(chocopyc PRIVATE ChocoPyCompiler)

option(ENABLE_TESTS "Enable unit tests" OFF)

if(ENABLE_TESTS)
  include(FetchContent)
  FetchContent_Declare(
    googletest

    # Specify the commit you depend on and update it regularly.
    URL https://github.com/google/googletest/archive/5376968f6948923e2411081fd9372e71a59d8e77.zip
  )
  # For Windows: Prevent overriding the parent project's compiler/linker settings
  set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
  FetchContent_MakeAvailable(googletest)

  enable_testing()

  add_subdirectory(tests)
endif()
