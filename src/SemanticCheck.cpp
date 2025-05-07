#include "SemanticCheck.h"

namespace chocopy {

SemanticCheckVisitor::SemanticCheckVisitor() {}

SemanticCheckVisitor::~SemanticCheckVisitor() {}

std::vector<SemanticError>
SemanticCheckVisitor::check(const ProgramAST& program) {
  program.accept(*this);
  return errors;
}

void SemanticCheckVisitor::visitProgram(const ProgramAST& program) {}

void SemanticCheckVisitor::visitLiteralNumber(
    const LiteralNumberAST& literalNumber) {}

void SemanticCheckVisitor::visitLiteralTrue(const LiteralTrueAST& literalTrue) {
}

void SemanticCheckVisitor::visitLiteralFalse(
    const LiteralFalseAST& literalFalse) {}

void SemanticCheckVisitor::visitLiteralString(
    const LiteralStringAST& literalString) {}

void SemanticCheckVisitor::visitCallExpr(const CallExprAST& callExpr) {}

} // namespace chocopy
