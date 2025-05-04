#include <iostream>
#include <memory>

#include "Lexer.h"
#include "Parser.h"
#include "CodeGen.h"

#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ErrorOr.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"

static llvm::cl::opt<std::string>
    inputFilename(llvm::cl::Positional, llvm::cl::desc("<input chocopy file>"),
                  llvm::cl::init("-"), llvm::cl::value_desc("filename"));

void parseInputFile(llvm::StringRef filename) {
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> fileOrErr =
      llvm::MemoryBuffer::getFileOrSTDIN(filename);
  if (std::error_code ec = fileOrErr.getError()) {
    llvm::errs() << "Could not open input file: " << ec.message() << "\n";
    return;
  }
  auto buffer = fileOrErr.get()->getBuffer();
  chocopy::LexerBuffer lexer(buffer.begin(), buffer.end(),
                             std::string(filename));
  chocopy::Parser parser(lexer);
  std::unique_ptr<chocopy::ProgramAST> program = parser.parseProgram();

  chocopy::LLVMCodeGenVisitor codegen;
  codegen.codeGen(*program, filename);
}

int main(int argc, char** argv) {
  llvm::cl::ParseCommandLineOptions(argc, argv, "chocopy compiler\n");
  parseInputFile(inputFilename);
}
