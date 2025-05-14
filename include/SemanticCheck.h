#ifndef CHOCOPY_SEMANTIC_H
#define CHOCOPY_SEMANTIC_H

#include <algorithm>

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
  std::vector<SemanticError> checkInheritance(const ProgramAST& program);

  void visitProgram(const ProgramAST& program) override;
  void visitClass(const ClassAST& clazz) override;
  void visitFunction(const FunctionAST& func) override;
  void visitLiteralNumber(const LiteralNumberAST& literalNumber) override;
  void visitLiteralTrue(const LiteralTrueAST& literalTrue) override;
  void visitLiteralFalse(const LiteralFalseAST& literalFalse) override;
  void visitLiteralString(const LiteralStringAST& literalString) override;
  void visitLiteralNone(const LiteralNoneAST& literalNone) override;
  void visitCallExpr(const CallExprAST& callExpr) override;
  void visitIdExpr(const IdExprAST& idExpr) override;
  void visitBinaryExpr(const BinaryExprAST& binaryExpr) override;
  void visitVarDef(const VarDefAST& varDef) override;
  void visitTypedVar(const TypedVarAST& typedVar) override;

  void
  visitSimpleStmtAssign(const SimpleStmtAssignAST& simpleStmtAssign) override;
  void visitSimpleStmtExpr(const SimpleStmtExprAST& simpleStmtExpr) override;

private:
  bool isDefinedType(const llvm::StringRef typeName) {
    return std::find(definedClassIds.begin(), definedClassIds.end(),
                     typeName) != definedClassIds.end();
  }
  std::vector<SemanticError> errors;
  std::vector<std::string> definedClassIds = {"object", "str", "bool", "int"};
  std::unordered_map<std::string, std::string> globalVarToType;

  ClassAST* currentClass = nullptr;
  FunctionAST* currentFunction = nullptr;

};

} // namespace chocopy

#endif // CHOCOPY_SEMANTIC_H
