#!/usr/bin/env bash
# Builds the LLVM and lld libraries chocopyc links against as WebAssembly,
# with Emscripten, so chocopyc itself can be compiled to run in the browser.
# Only the WebAssembly target and lld's wasm linker are needed there.
#
# Usage: source ~/emsdk/emsdk_env.sh && tools/build-llvm-wasm.sh
#
# The source, build and install trees go in $CHOCOPY_LLVM_WASM_DIR (default
# ~/.cache/chocopy-llvm-wasm); the libraries are installed into its install/
# directory, which CMake finds with -DLLVM_DIR=<dir>/install/lib/cmake/llvm.
set -euo pipefail

LLVM_VERSION=17.0.6
DIR=${CHOCOPY_LLVM_WASM_DIR:-$HOME/.cache/chocopy-llvm-wasm}
# A native llvm-tblgen of the same version generates code during the build
NATIVE_LLVM_CONFIG=${NATIVE_LLVM_CONFIG:-$(brew --prefix llvm@17 2>/dev/null)/bin/llvm-config}

if ! command -v emcmake > /dev/null; then
  echo "emcmake not found: source emsdk_env.sh first" >&2
  exit 1
fi
if [ ! -x "$NATIVE_LLVM_CONFIG" ]; then
  echo "Set NATIVE_LLVM_CONFIG to the llvm-config of a native LLVM $LLVM_VERSION" >&2
  exit 1
fi
NATIVE_BIN=$("$NATIVE_LLVM_CONFIG" --bindir)

mkdir -p "$DIR"
cd "$DIR"

if [ ! -d src ]; then
  archive=llvm-project-$LLVM_VERSION.src.tar.xz
  curl -sSLO "https://github.com/llvm/llvm-project/releases/download/llvmorg-$LLVM_VERSION/$archive"
  tar -xJf "$archive" llvm-project-$LLVM_VERSION.src/{llvm,lld,cmake,libunwind/include,third-party}
  mv llvm-project-$LLVM_VERSION.src src
  rm "$archive"
fi

emcmake cmake -S src/llvm -B build -G Ninja \
  -DCMAKE_BUILD_TYPE=MinSizeRel \
  -DCMAKE_INSTALL_PREFIX="$DIR/install" \
  -DLLVM_TARGETS_TO_BUILD=WebAssembly \
  -DLLVM_ENABLE_PROJECTS=lld \
  -DLLVM_HOST_TRIPLE=wasm32-unknown-emscripten \
  -DLLVM_DEFAULT_TARGET_TRIPLE=wasm32-unknown-wasi \
  -DLLVM_TABLEGEN="$NATIVE_BIN/llvm-tblgen" \
  -DLLVM_ENABLE_THREADS=OFF \
  -DLLVM_ENABLE_ZLIB=OFF \
  -DLLVM_ENABLE_ZSTD=OFF \
  -DLLVM_ENABLE_LIBXML2=OFF \
  -DLLVM_ENABLE_TERMINFO=OFF \
  -DLLVM_ENABLE_LIBEDIT=OFF \
  -DLLVM_ENABLE_LIBPFM=OFF \
  -DLLVM_ENABLE_BACKTRACES=OFF \
  -DLLVM_ENABLE_CRASH_OVERRIDES=OFF \
  -DLLVM_BUILD_TOOLS=OFF \
  -DLLVM_BUILD_UTILS=OFF \
  -DLLVM_INCLUDE_TESTS=OFF \
  -DLLVM_INCLUDE_BENCHMARKS=OFF \
  -DLLVM_INCLUDE_EXAMPLES=OFF \
  -DLLVM_INCLUDE_DOCS=OFF

cmake --build build
cmake --install build
