#include <iostream>
#include <memory>

#include "lexer.h"

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
}

int main(int argc, char** argv) {
  llvm::StringRef strRef = "ChocoPy Compiler";
  std::cout << strRef.str() << std::endl;
  bool result = llvm::cl::ParseCommandLineOptions(argc, argv, "chocopy compiler\n");
  std::cout << inputFilename << std::endl;
  parseInputFile(inputFilename);
}
