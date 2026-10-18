#ifndef CHOCOPY_BACKEND_H
#define CHOCOPY_BACKEND_H

#include <memory>
#include <string>

#include "llvm/ADT/StringRef.h"
#include "llvm/IR/Module.h"
#include "llvm/Target/TargetMachine.h"

namespace chocopy {

// Target triple of the machine chocopyc runs on, normalized so that object
// files and the linker agree on the platform version.
std::string getHostTargetTriple();

// Target triple for WebAssembly output: a WASI command module.
std::string getWasmTargetTriple();

// TargetMachine for the given triple (the host or wasm). Returns nullptr and
// sets error on failure.
std::unique_ptr<llvm::TargetMachine>
createTargetMachine(llvm::StringRef triple, std::string& error);

// Emits a native object file for the module into outputPath.
bool emitObjectFile(llvm::Module& module, llvm::TargetMachine& targetMachine,
                    llvm::StringRef outputPath, std::string& error);

// Links an object file with the ChocoPy runtime library and the system C
// library into an executable, using the embedded lld linker. The runtime
// library is embedded in chocopyc. For wasm targets the output is a .wasm
// module linked with the freestanding wasm runtime and no C library.
bool linkExecutable(llvm::StringRef objectPath, llvm::StringRef outputPath,
                    llvm::StringRef targetTriple, std::string& error);

} // namespace chocopy

#endif // CHOCOPY_BACKEND_H
