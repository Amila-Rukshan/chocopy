#include <iostream>
#include <memory>

#include "CodeGen.h"
#include "Lexer.h"
#include "Parser.h"
#include "SemanticCheck.h"

#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ErrorOr.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"

static llvm::cl::opt<std::string>
    inputFilename(llvm::cl::Positional, llvm::cl::desc("<input chocopy file>"),
                  llvm::cl::init("-"), llvm::cl::value_desc("filename"));

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
  codegen.printLLVMBitCode(inputFilename.data());
}
