#ifndef CHOCOPY_SEMANTIC_H
#define CHOCOPY_SEMANTIC_H

#include "AST.h"

namespace chocopy {

class SemanticError {
public:
  SemanticError(int line, int column, std::string message)
      : line(line), column(column), message(message) {}
  std::string get_error_message() const {
    return ":" + std::to_string(line) + ":" + std::to_string(column) + ": " +
           message;
  }

private:
  int line;
  int column;
  std::string message;
};

class SemanticCheckVisitor : public ASTVisitor {
public:
  SemanticCheckVisitor();
  ~SemanticCheckVisitor();

  std::vector<SemanticError> check(const ProgramAST& program);

  void visitProgram(const ProgramAST& program) override;
  void visitLiteralNumber(const LiteralNumberAST& literalNumber) override;
  void visitLiteralTrue(const LiteralTrueAST& literalTrue) override;
  void visitLiteralFalse(const LiteralFalseAST& literalFalse) override;
  void visitLiteralString(const LiteralStringAST& literalString) override;
  void visitCallExpr(const CallExprAST& callExpr) override;

private:
  std::vector<SemanticError> errors;
};

} // namespace chocopy

#endif // CHOCOPY_SEMANTIC_H
