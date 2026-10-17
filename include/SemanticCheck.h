#ifndef CHOCOPY_SEMANTIC_H
#define CHOCOPY_SEMANTIC_H

#include <algorithm>
#include <stack>
#include <unordered_set>

#include "AST.h"
#include "Utils.h"

namespace chocopy {

class SemanticError {
public:
  SemanticError(int line, int column, std::string message)
      : line(line), column(column), message(message) {}
  std::string getErrorMsg() const {
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
  void visitListLiteralExpr(const ListLiteralExprAST& listLiteralExpr) override;

  void visitCallExpr(const CallExprAST& callExpr) override;
  void visitIdExpr(const IdExprAST& idExpr) override;
  void visitBinaryExpr(const BinaryExprAST& binaryExpr) override;
  void visitUnaryExpr(const UnaryExprAST& unaryExpr) override;
  void visitVarDef(const VarDefAST& varDef) override;
  void visitTypedVar(const TypedVarAST& typedVar) override;
  void
  visitSimpleStmtReturn(const SimpleStmtReturnAST& simpleStmtReturn) override;

  void
  visitSimpleStmtAssign(const SimpleStmtAssignAST& simpleStmtAssign) override;
  void visitSimpleStmtExpr(const SimpleStmtExprAST& simpleStmtExpr) override;
  void visitIfElseExpr(const IfElseExprAST& ifElseExpr) override;

  void visitStmtIf(const StmtIfAST& stmtIf) override;
  void visitStmtWhile(const StmtWhileAST& stmtWhile) override;
  void visitStmtFor(const StmtForAST& stmtFor) override;

private:
  void registerFunction(const FunctionAST& func, const std::string& prefix);
  const FunctionAST* resolveFunction(const std::string& name) const;
  ClassAST* findClass(const std::string& classId) const;

  void checkGlobalNames(const ProgramAST& program);
  void checkCallArgs(const CallExprAST& callExpr, const FunctionAST& func,
                     size_t hiddenParams);
  bool alwaysReturns(const std::vector<std::unique_ptr<StmtAST>>& stmts);
  bool isValidTypeAnnotation(const TypeAST* type);
  void checkNotObjectAnnotation(const TypeAST* type);

  // T1 <= T2: ordinary conformance in the class hierarchy
  bool isSubClassOf(const std::string& subType, const std::string& superType);
  // T1 <=a T2: assignment compatibility
  bool isAssignable(const std::string& subType, const std::string& superType);

  std::string typeUnion(const std::string& lhsType, const std::string& rhsType);

  const FunctionAST* currentFunction() const;

  bool isDefinedType(const llvm::StringRef typeName) {
    return std::find(definedClassIds.begin(), definedClassIds.end(),
                     typeName) != definedClassIds.end();
  }
  std::vector<SemanticError> errors;
  std::vector<std::string> definedClassIds = {"object", "str", "bool", "int"};
  std::vector<std::string> primitiveTypes = {"str", "bool", "int"};
  std::unordered_map<std::string, ClassAST*> definedClasses;
  std::unordered_map<std::string, const FunctionAST*> definedFunctions;
  std::unordered_map<std::string, std::string> globalVarToType;
  std::unordered_map<std::string, std::string> localVarToType;
  // Variables the current function may assign to: its parameters, locals and
  // global/nonlocal declarations. Inherited bindings are read-only.
  std::unordered_set<std::string> currentScopeVars;

  ClassAST* currentClass = nullptr;
  std::stack<const FunctionAST*> functionStack;
};

inline const VarDefAST* lookupAttributeInHierarchy(const ClassAST* clazz,
                                                   const std::string& attrId) {
  while (clazz && clazz->getId() != "object") {
    if (clazz->hasAttribute(attrId)) {
      return clazz->getAttribute(attrId);
    }
    clazz = clazz->getParentClass();
  }
  return nullptr;
}

inline const FunctionAST* lookupMethodInHierarchy(const ClassAST* clazz,
                                                  const std::string& methodId) {
  while (clazz && clazz->getId() != "object") {
    if (clazz->hasMethod(methodId)) {
      return clazz->getMethod(methodId);
    }
    clazz = clazz->getParentClass();
  }
  return nullptr;
}

} // namespace chocopy

#endif // CHOCOPY_SEMANTIC_H
