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

// TargetMachine for the host. Returns nullptr and sets error on failure.
std::unique_ptr<llvm::TargetMachine> createHostTargetMachine(std::string& error);

// Emits a native object file for the module into outputPath.
bool emitObjectFile(llvm::Module& module, llvm::TargetMachine& targetMachine,
                    llvm::StringRef outputPath, std::string& error);

// Links an object file with the ChocoPy runtime library and the system C
// library into an executable, using the embedded lld linker.
bool linkExecutable(llvm::StringRef objectPath, llvm::StringRef runtimeLibPath,
                    llvm::StringRef outputPath, llvm::StringRef targetTriple,
                    std::string& error);

// Path of the runtime library built alongside chocopyc.
std::string defaultRuntimeLibPath();

} // namespace chocopy

#endif // CHOCOPY_BACKEND_H
