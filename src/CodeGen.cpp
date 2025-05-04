#include <filesystem>
#include <iostream>

#include "CodeGen.h"

namespace chocopy {

LLVMCodeGenVisitor::LLVMCodeGenVisitor()
    : context(std::make_unique<llvm::LLVMContext>()),
      builder(std::make_unique<llvm::IRBuilder<>>(*context)) {}

LLVMCodeGenVisitor::~LLVMCodeGenVisitor() {}

void LLVMCodeGenVisitor::codeGen(const chocopy::ProgramAST& program,
                                 llvm::StringRef programPath) {
  std::filesystem::path path(programPath.data());
  std::string fileName = path.filename().string();

  module = std::make_unique<llvm::Module>(fileName, *context);

  program.accept(*this);
}

void LLVMCodeGenVisitor::visitProgram(const ProgramAST& program) {
  std::cout << "Visiting Program" << std::endl;
}

} // namespace chocopy
