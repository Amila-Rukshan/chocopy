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

option(CHOCOPY_STATIC_LINK
  "Link LLVM, lld, zstd and the C++ runtime statically (redistributable chocopyc)" OFF)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

# Runtime library linked into every compiled ChocoPy program
add_subdirectory(std)

# The runtime library and the macOS libSystem stub are compiled into
# chocopyc, so it ships as a single executable
set(_embedded_runtime ${CMAKE_BINARY_DIR}/EmbeddedRuntime.cpp)
add_custom_command(
  OUTPUT ${_embedded_runtime}
  COMMAND ${CMAKE_COMMAND}
          -DOUTPUT=${_embedded_runtime}
          -DRUNTIME_LIB=$<TARGET_FILE:ChocopySupport>
          -DLIBSYSTEM_STUB=${CMAKE_SOURCE_DIR}/std/libSystem.tbd
          -P ${CMAKE_SOURCE_DIR}/cmake/EmbedRuntime.cmake
  DEPENDS ChocopySupport $<TARGET_FILE:ChocopySupport>
          ${CMAKE_SOURCE_DIR}/std/libSystem.tbd
          ${CMAKE_SOURCE_DIR}/cmake/EmbedRuntime.cmake
  COMMENT "Embedding the ChocoPy runtime")

# Front end, LLVM code generation and the embedded linker
add_library(ChocoPyCompiler STATIC
  src/Backend.cpp
  src/CodeGen.cpp
  src/SemanticCheck.cpp
  ${_embedded_runtime})

target_compile_definitions(ChocoPyCompiler
  PRIVATE
  CHOCOPY_MACOS_MIN_VERSION="${CHOCOPY_MACOS_MIN_VERSION}")

target_include_directories(ChocoPyCompiler PUBLIC ${LLD_INCLUDE_DIRS})

set(_lld_libs lldCommon lldELF lldMachO)
if(CHOCOPY_STATIC_LINK)
  # On Linux, zlib and terminfo are linked statically too, leaving only libc
  # (macOS ships both as system libraries).
  if(NOT APPLE)
    find_library(_zlib_static NAMES libz.a REQUIRED)
    find_library(_tinfo_static NAMES libtinfo.a REQUIRED)
  endif()

  # The exported lld and LLVM targets link libLLVM and shared libraries; point
  # them at the static libraries instead.
  foreach(_target ${_lld_libs} LLVMSupport)
    get_target_property(_libs ${_target} INTERFACE_LINK_LIBRARIES)
    list(REMOVE_ITEM _libs LLVM)
    list(TRANSFORM _libs REPLACE "^zstd::libzstd_shared$" "zstd::libzstd_static")
    if(NOT APPLE)
      list(TRANSFORM _libs REPLACE "^ZLIB::ZLIB$" "${_zlib_static}")
      list(TRANSFORM _libs REPLACE "^Terminfo::terminfo$" "${_tinfo_static}")
    endif()
    set_target_properties(${_target} PROPERTIES INTERFACE_LINK_LIBRARIES "${_libs}")
  endforeach()
  # LLVM components lld itself needs, normally provided by libLLVM. lld's
  # drivers initialize every target for LTO, so all of them are linked.
  set(_lld_components
    ${LLVM_TARGETS_TO_BUILD} BinaryFormat BitReader BitWriter CodeGen
    DebugInfoDWARF Demangle LTO ObjCARCOpts Object Option TextAPI)
elseif(LLVM_LINK_LLVM_DYLIB)
  set(_llvm_use_shared USE_SHARED)
endif()

# lld before the LLVM libraries it uses: GNU ld resolves static archives in
# a single left-to-right pass
target_link_libraries(ChocoPyCompiler PUBLIC ${_lld_libs})

llvm_config(ChocoPyCompiler ${_llvm_use_shared}
  native Core Support TargetParser MC Target Passes ${_lld_components})

add_executable(chocopyc ChocoPy.cpp)

target_link_libraries(chocopyc PRIVATE ChocoPyCompiler)

if(CHOCOPY_STATIC_LINK AND NOT APPLE)
  target_link_options(chocopyc PRIVATE -static-libstdc++ -static-libgcc)
endif()

install(TARGETS chocopyc RUNTIME DESTINATION bin)

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
