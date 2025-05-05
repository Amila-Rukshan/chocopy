#ifndef CHOCOPY_CODEGEN_H
#define CHOCOPY_CODEGEN_H

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"

#include "AST.h"

namespace chocopy {

class LLVMCodeGenVisitor : public ASTVisitor {
public:
  LLVMCodeGenVisitor();
  ~LLVMCodeGenVisitor();

  void codeGen(const ProgramAST& program, llvm::StringRef program_path);
  void codeGenMainFunc(const std::vector<std::unique_ptr<StmtAST>>& stmts);
  void printLLVMBitCode(llvm::StringRef outputPath) const;

  void visitProgram(const ProgramAST& program) override;
  void visitLiteralNumber(const LiteralNumberAST& literalNumber) override;
  void visitLiteralTrue(const LiteralTrueAST& literalTrue) override;
  void visitLiteralFalse(const LiteralFalseAST& literalFalse) override;

private:
  std::unique_ptr<llvm::LLVMContext> context;
  std::unique_ptr<llvm::IRBuilder<>> builder;
  std::unique_ptr<llvm::Module> module;
};

} // namespace chocopy

#endif // CHOCOPY_CODEGEN_H
