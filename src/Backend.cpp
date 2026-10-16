#include <vector>

#include "Backend.h"

#include "lld/Common/Driver.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/TargetParser/Host.h"
#include "llvm/TargetParser/Triple.h"

LLD_HAS_DRIVER(elf)
LLD_HAS_DRIVER(macho)

namespace chocopy {

namespace {

bool fileExists(const std::string& path) {
  return !path.empty() && llvm::sys::fs::exists(path);
}

std::vector<std::string> machOLinkArgs(const llvm::Triple& triple,
                                       llvm::StringRef objectPath,
                                       llvm::StringRef runtimeLibPath,
                                       llvm::StringRef outputPath) {
#ifdef __APPLE__
  std::string arch = triple.isAArch64() ? "arm64" : triple.getArchName().str();
  // The SDK version is only recorded in the binary; use the deployment target
  // since no SDK is involved in the link.
  return {"ld64.lld",
          "-arch",
          arch,
          "-platform_version",
          "macos",
          CHOCOPY_MACOS_MIN_VERSION,
          CHOCOPY_MACOS_MIN_VERSION,
          "-o",
          outputPath.str(),
          objectPath.str(),
          runtimeLibPath.str(),
          CHOCOPY_MACOS_LIBSYSTEM_STUB};
#else
  return {};
#endif
}

std::vector<std::string> elfLinkArgs(llvm::StringRef objectPath,
                                     llvm::StringRef runtimeLibPath,
                                     llvm::StringRef outputPath) {
#ifdef __linux__
  std::string crtDir = CHOCOPY_LINUX_CRT_DIR;
  std::string gccDir = CHOCOPY_LINUX_GCC_DIR;

  std::vector<std::string> args = {"ld.lld",
                                   "--eh-frame-hdr",
                                   "-pie",
                                   "-dynamic-linker",
                                   CHOCOPY_LINUX_DYNAMIC_LINKER,
                                   "-o",
                                   outputPath.str(),
                                   crtDir + "/Scrt1.o",
                                   crtDir + "/crti.o"};
  // crtbegin/crtend come from GCC and are optional for plain C code.
  bool hasGccCrt = fileExists(gccDir + "/crtbeginS.o");
  if (hasGccCrt)
    args.push_back(gccDir + "/crtbeginS.o");

  args.push_back(objectPath.str());
  args.push_back(runtimeLibPath.str());
  args.push_back("-L" + crtDir);
  if (!gccDir.empty())
    args.push_back("-L" + gccDir);
  args.push_back("-lc");

  if (hasGccCrt)
    args.push_back(gccDir + "/crtendS.o");
  args.push_back(crtDir + "/crtn.o");
  return args;
#else
  return {};
#endif
}

} // namespace

std::string getHostTargetTriple() {
  llvm::Triple triple(llvm::sys::getProcessTriple());
#ifdef __APPLE__
  // Use an explicit macOS version so objects match the -platform_version
  // passed to the linker.
  if (triple.isOSDarwin())
    triple.setOSName(std::string("macosx") + CHOCOPY_MACOS_MIN_VERSION);
#endif
  return triple.str();
}

std::unique_ptr<llvm::TargetMachine>
createHostTargetMachine(std::string& error) {
  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmPrinter();

  std::string triple = getHostTargetTriple();
  const llvm::Target* target =
      llvm::TargetRegistry::lookupTarget(triple, error);
  if (!target)
    return nullptr;

  llvm::TargetOptions options;
  return std::unique_ptr<llvm::TargetMachine>(target->createTargetMachine(
      triple, "generic", "", options, llvm::Reloc::PIC_));
}

bool emitObjectFile(llvm::Module& module, llvm::TargetMachine& targetMachine,
                    llvm::StringRef outputPath, std::string& error) {
  std::error_code ec;
  // Removed on destruction unless keep() is called, so failures leave no
  // partial output behind.
  llvm::ToolOutputFile dest(outputPath, ec, llvm::sys::fs::OF_None);
  if (ec) {
    error = "Could not open " + outputPath.str() + ": " + ec.message();
    return false;
  }

  llvm::legacy::PassManager pm;
  if (targetMachine.addPassesToEmitFile(pm, dest.os(), nullptr,
                                        llvm::CodeGenFileType::CGFT_ObjectFile)) {
    error = "Target cannot emit object files";
    return false;
  }
  pm.run(module);
  dest.keep();
  return true;
}

bool linkExecutable(llvm::StringRef objectPath, llvm::StringRef runtimeLibPath,
                    llvm::StringRef outputPath, llvm::StringRef targetTriple,
                    std::string& error) {
  if (!fileExists(runtimeLibPath.str())) {
    error = "ChocoPy runtime library not found: " + runtimeLibPath.str();
    return false;
  }

  llvm::Triple triple(targetTriple);
  std::vector<std::string> args;
  if (triple.isOSDarwin())
    args = machOLinkArgs(triple, objectPath, runtimeLibPath, outputPath);
  else if (triple.isOSLinux())
    args = elfLinkArgs(objectPath, runtimeLibPath, outputPath);

  if (args.empty()) {
    error = "Linking is not supported for target " + targetTriple.str();
    return false;
  }

  std::vector<const char*> argv;
  for (const auto& arg : args)
    argv.push_back(arg.c_str());

  std::string linkerOutput;
  llvm::raw_string_ostream linkerOS(linkerOutput);
  lld::Result result =
      lld::lldMain(argv, linkerOS, linkerOS,
                   {{lld::Gnu, &lld::elf::link}, {lld::Darwin, &lld::macho::link}});
  linkerOS.flush();

  if (result.retCode != 0) {
    error = "Linking failed:\n" + linkerOutput;
    return false;
  }
  if (!linkerOutput.empty())
    llvm::errs() << linkerOutput;
  return true;
}

std::string defaultRuntimeLibPath() { return CHOCOPY_RUNTIME_LIB; }

} // namespace chocopy
