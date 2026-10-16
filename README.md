This tree is to implement **ChocoPy** language using LLVM compiler infrastructure.

## ChocoPy v2.2

See [chocopy.pdf](./chocopy.pdf) for a full description of ChocoPy language with Type Checking and Operational Semantics.

## Related content

### Blog posts

- [Building Dynamic Dispatch in ChocoPy: From Concept to Code](https://developerdiary.me/building-dynamic-dispatch-in-chocopy-from-concept-to-code/)

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

Add `-DENABLE_TESTS=ON` to the configure step to build the unit tests. The VSCode `Debug ChocoPy` and test launch configurations also work locally.

On macOS, generated programs link against [`std/libSystem.tbd`](./std/libSystem.tbd), a stub listing the C library symbols ChocoPy uses, so no macOS SDK is needed to link them.

### Using the compiler

`chocopyc` is a complete compiler: it generates native code with LLVM and links it with the ChocoPy runtime and the system C library using an embedded lld, so no separate `clang` step is needed.

```sh
./build/chocopyc testprograms/choco.py      # writes the executable testprograms/choco
./testprograms/choco

./build/chocopyc prog.py -o prog            # choose the output name
./build/chocopyc prog.py --emit=obj         # object file (prog.o)
./build/chocopyc prog.py --emit=llvm        # LLVM IR (prog.py.ll)
```

## TODOS

- [x] Lexing
- [x] Parsing
- [x] Classes, methods and attributes (with default values)
- [x] `\_\_init\_\_` method (aka class constructor) to initilize attributes
- [x] Method calling with dynamic dispatch
- [x] Builtin 'print' function
- [x] Builtin 'str' type
- [ ] Complete type checking
- [x] Control flows - `if-elif-else`, `while` and `for`
- [x] Binary and unary oprations for int, bool and str types
- [x] List data type support
- [x] Nested functions with closures
