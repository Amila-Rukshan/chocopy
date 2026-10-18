add_library(ChocopySupport STATIC string.c runtime.c)

# Generated programs are linked as position-independent executables
set_target_properties(ChocopySupport PROPERTIES POSITION_INDEPENDENT_CODE ON)

if(APPLE)
  target_compile_options(ChocopySupport PRIVATE -mmacosx-version-min=${CHOCOPY_MACOS_MIN_VERSION})
endif()

# Runtime for programs compiled to WebAssembly. The host C compiler may not
# target wasm (Apple clang does not), so it is built with LLVM's own clang.
find_program(CHOCOPY_WASM_CLANG NAMES clang clang-17
  HINTS ${LLVM_TOOLS_BINARY_DIR} NO_DEFAULT_PATH)
find_program(CHOCOPY_WASM_CLANG NAMES clang-17 clang)
if(NOT CHOCOPY_WASM_CLANG)
  message(FATAL_ERROR "clang with the WebAssembly target is needed to build the wasm runtime")
endif()

set(CHOCOPY_WASM_RUNTIME ${CMAKE_CURRENT_BINARY_DIR}/wasm-runtime.o PARENT_SCOPE)
add_custom_command(
  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/wasm-runtime.o
  COMMAND ${CHOCOPY_WASM_CLANG} --target=wasm32 -O2 -ffreestanding -fno-builtin
          -nostdlib -c ${CMAKE_CURRENT_SOURCE_DIR}/wasm/runtime.c
          -o ${CMAKE_CURRENT_BINARY_DIR}/wasm-runtime.o
  DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/wasm/runtime.c
  COMMENT "Building the ChocoPy wasm runtime")
add_custom_target(ChocopyWasmRuntime DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/wasm-runtime.o)
