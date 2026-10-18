This tree is to implement **ChocoPy** language using LLVM compiler infrastructure.

## ChocoPy v2.2

See [chocopy.pdf](./chocopy.pdf) for a full description of ChocoPy language with Type Checking and Operational Semantics.

## Related content

### Blog posts

- [Building Dynamic Dispatch in ChocoPy: From Concept to Code](https://developerdiary.me/building-dynamic-dispatch-in-chocopy-from-concept-to-code/)

## Install a release

Prebuilt compilers for Linux and macOS (x86_64 and arm64) are attached to each [GitHub release](https://github.com/Amila-Rukshan/chocopy/releases). `chocopyc` is a single self-contained executable (the ChocoPy runtime is embedded in it); copy it anywhere on your `PATH`:

```sh
tar -xzf chocopy-<version>-<os>-<arch>.tar.gz
cp chocopy-<version>-<os>-<arch>/chocopyc /usr/local/bin/
chocopyc hello.py && ./hello
```

- **Linux** needs glibc 2.38+ and the C library development files (`libc6-dev` on Debian/Ubuntu, `glibc-devel` on Fedora), which provide the startup files programs are linked with.
- **macOS** needs macOS 15+. The binaries are not notarized, so if you download with a browser, clear the quarantine flag first: `xattr -dr com.apple.quarantine chocopy-<version>-macos-<arch>`.

To publish a release, push a version tag (`git tag v1.0.0 && git push origin v1.0.0`). The [release workflow](.github/workflows/release.yml) runs the tests on all four platforms, builds statically linked compilers, smoke-tests each package and attaches them to the release.

## How to run?

A Dev container is setup for linux OS with the llvm dependancies. Just open the project in `VSCode` and from the Debug window, run `Debug ChocoPy`.

### Running locally on macOS

Install LLVM 17 (includes lld) and the build tools with Homebrew:

```sh
brew install llvm@17 cmake ninja
```

Then build the compiler:

```sh
cmake -S . -B build -G Ninja -DCMAKE_BUILD_TYPE=Debug
cmake --build build
```

Add `-DENABLE_TESTS=ON` to the configure step to build the unit tests (run them with `ctest --test-dir build`). The VSCode `Debug ChocoPy` and test launch configurations also work locally.

`-DCHOCOPY_STATIC_LINK=ON` links LLVM and lld statically, producing a redistributable compiler; `cmake --install build --prefix <dir>` installs it as `<dir>/bin/chocopyc`.

On macOS, generated programs link against [`std/libSystem.tbd`](./std/libSystem.tbd), a stub listing the C library symbols ChocoPy uses, so no macOS SDK is needed to link them. The stub and the runtime library built from `std/` are embedded in `chocopyc` at build time.

### Using the compiler

`chocopyc` is a complete compiler: it generates native code with LLVM and links it with the ChocoPy runtime and the system C library using an embedded lld, so no separate `clang` step is needed.

```sh
./build/bin/chocopyc testprograms/choco.py      # writes the executable testprograms/choco
./testprograms/choco

./build/bin/chocopyc prog.py -o prog            # choose the output name
./build/bin/chocopyc prog.py --emit=obj         # object file (prog.o)
./build/bin/chocopyc prog.py --emit=llvm        # LLVM IR (prog.py.ll)
```

### WebAssembly

`--target=wasm32` compiles to a WebAssembly module instead, linked with the freestanding runtime in [`std/wasm/runtime.c`](./std/wasm/runtime.c) (no C library). Modules only import WASI's `fd_write` and `proc_exit`, so they run under any WASI host or in the browser:

```sh
./build/bin/chocopyc --target=wasm32 prog.py   # writes prog.wasm
node tools/run-wasm.mjs prog.wasm              # or: wasmtime prog.wasm
```

In the browser, [`web/chocopy-wasm.js`](./web/chocopy-wasm.js) provides those two imports.

### Compiler in the browser

`chocopyc` itself can be built to WebAssembly with [Emscripten](https://emscripten.org), so the playground in [`web/`](./web) compiles and runs ChocoPy entirely in the browser. It needs LLVM and lld built for wasm first; [`tools/build-llvm-wasm.sh`](./tools/build-llvm-wasm.sh) builds just the parts `chocopyc` uses (the WebAssembly target and lld's wasm linker) into `~/.cache/chocopy-llvm-wasm`:

```sh
source ~/emsdk/emsdk_env.sh
tools/build-llvm-wasm.sh                        # once; LLVM 17 for wasm

LLVM_WASM=~/.cache/chocopy-llvm-wasm/install
emcmake cmake -S . -B build-wasm -G Ninja -DCMAKE_BUILD_TYPE=MinSizeRel \
  -DLLVM_DIR=$LLVM_WASM/lib/cmake/llvm -DLLD_DIR=$LLVM_WASM/lib/cmake/lld
cmake --build build-wasm                        # copies chocopyc into web/compiler/

python3 -m http.server -d web                   # open http://localhost:8000
```

The native `llvm@17` install is still needed: its `llvm-tblgen` is used while building LLVM, and its `clang` builds the wasm runtime. The wasm build of `chocopyc` produces the same modules as the native `chocopyc --target=wasm32`; `node tools/test-browser-compiler.mjs testprograms/chocopy_*.py` checks that for every test program.

## TODOS

- [x] Lexing
- [x] Parsing
- [x] Classes, methods and attributes (with default values)
- [x] `\_\_init\_\_` method (aka class constructor) to initilize attributes
- [x] Method calling with dynamic dispatch
- [x] Builtin 'print' function
- [x] Builtin 'str' type
- [x] Complete type checking
- [x] Control flows - `if-elif-else`, `while` and `for`
- [x] Binary and unary oprations for int, bool and str types
- [x] List data type support
- [x] Nested functions with closures
