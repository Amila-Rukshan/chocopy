#include <iostream>
#include <memory>

#include "Backend.h"
#include "CodeGen.h"
#include "Lexer.h"
#include "Parser.h"
#include "SemanticCheck.h"

#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ErrorOr.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/raw_ostream.h"

static llvm::cl::OptionCategory chocopyCategory("ChocoPy options");

static llvm::cl::opt<std::string>
    inputFilename(llvm::cl::Positional, llvm::cl::desc("<input chocopy file>"),
                  llvm::cl::init("-"), llvm::cl::value_desc("filename"),
                  llvm::cl::cat(chocopyCategory));

static llvm::cl::opt<std::string>
    outputFilename("o", llvm::cl::desc("Output file"),
                   llvm::cl::value_desc("filename"),
                   llvm::cl::cat(chocopyCategory));

enum class EmitKind { Exe, Obj, LLVM };

static llvm::cl::opt<EmitKind> emitKind(
    "emit", llvm::cl::desc("Kind of output to produce"),
    llvm::cl::init(EmitKind::Exe),
    llvm::cl::values(
        clEnumValN(EmitKind::Exe, "exe", "Native executable (default)"),
        clEnumValN(EmitKind::Obj, "obj", "Native object file"),
        clEnumValN(EmitKind::LLVM, "llvm", "LLVM IR (.ll)")),
    llvm::cl::cat(chocopyCategory));

static llvm::cl::opt<std::string>
    runtimeLib("runtime-lib", llvm::cl::desc("ChocoPy runtime library to link"),
               llvm::cl::init(chocopy::defaultRuntimeLibPath()),
               llvm::cl::value_desc("path"), llvm::cl::cat(chocopyCategory));

// foo.py -> foo (exe), foo.o (obj), foo.py.ll (llvm)
static std::string defaultOutputFilename(llvm::StringRef input, EmitKind kind) {
  if (input == "-")
    input = "a.py";
  if (kind == EmitKind::LLVM)
    return input.str() + ".ll";

  llvm::SmallString<128> output(input);
  if (llvm::sys::path::extension(output) == ".py")
    llvm::sys::path::replace_extension(output, "");
  else if (kind == EmitKind::Exe)
    output += ".out";
  if (kind == EmitKind::Obj)
    llvm::sys::path::replace_extension(output, "o");
  return output.str().str();
}

std::unique_ptr<llvm::MemoryBuffer> parseInputFile(llvm::StringRef filename) {
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> fileOrErr =
      llvm::MemoryBuffer::getFileOrSTDIN(filename);
  if (std::error_code ec = fileOrErr.getError()) {
    llvm::errs() << "Could not open input file: " << ec.message() << "\n";
    return nullptr;
  }
  return std::move(fileOrErr.get());
}

int main(int argc, char** argv) {
  llvm::cl::HideUnrelatedOptions(chocopyCategory);
  llvm::cl::ParseCommandLineOptions(argc, argv, "chocopy compiler\n");
  auto memBuffer = parseInputFile(inputFilename);
  if (!memBuffer) {
    return 1;
  }
  auto buffer = memBuffer->getBuffer();
  chocopy::LexerBuffer lexer(buffer.begin(), buffer.end(),
                             std::string(inputFilename));
  chocopy::Parser parser(lexer);
  std::unique_ptr<chocopy::ProgramAST> program = parser.parseProgram();

  chocopy::SemanticCheckVisitor semanticCheck;
  auto errors = semanticCheck.check(*program);
  if (!errors.empty()) {
    for (const auto& error : errors) {
      llvm::errs() << inputFilename << error.getErrorMsg();
    }
    return 1;
  }

  chocopy::LLVMCodeGenVisitor codegen(program.get(), inputFilename);
  codegen.codeGen();

  std::string output = outputFilename.empty()
                           ? defaultOutputFilename(inputFilename, emitKind)
                           : outputFilename.getValue();
  bool ok = false;
  switch (emitKind) {
  case EmitKind::Exe:
    ok = codegen.emitExecutable(output, runtimeLib);
    break;
  case EmitKind::Obj:
    ok = codegen.emitObjectFile(output);
    break;
  case EmitKind::LLVM:
    ok = codegen.writeLLVMIR(output);
    break;
  }
  return ok ? 0 : 1;
}
