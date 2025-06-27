#ifndef CHOCOPY_SEMANTIC_H
#define CHOCOPY_SEMANTIC_H

#include <algorithm>

#include "AST.h"

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

  inline std::string typeUnion(const std::string& lhsType,
                               const std::string& rhsType);
  inline bool isPrimitiveType(const std::string& type) {
    return std::find(primitiveTypes.begin(), primitiveTypes.end(), type) !=
           primitiveTypes.end();
  }
  inline bool isSubTypeOf(const std::string& subType,
                          const std::string& superType) {
    auto subPos = subType.find('[');
    auto superPos = superType.find('[');
    if (subPos == std::string::npos ^ superPos == std::string::npos)
      return false;
    if (subPos != std::string::npos && superPos != std::string::npos) {
      int dimSub = std::count(subType.begin(), subType.end(), '[');
      int dimSuper = std::count(superType.begin(), superType.end(), '[');
      if (dimSub != dimSuper)
        return false;
      auto subTypeBase = subType.substr(0, subPos);
      auto superTypeBase = superType.substr(0, superPos);
      if (subTypeBase == "<Empty>") {
        return true;
      }
      return isSubTypeOf(subTypeBase, superTypeBase);
    }

    if (subType == superType || subType == "<None>")
      return true;

    ClassAST* subClass = definedClasses[subType];
    ClassAST* superClass = definedClasses[superType];

    while (subClass && subClass->getId() != "object") {
      if (subClass->getId() == superClass->getId())
        return true;
      subClass = const_cast<ClassAST*>(subClass->getParentClass());
    }
    return false;
  }

  inline bool isListType(const std::string& type) {
    return type.find('[') != std::string::npos &&
           type.find(']') != std::string::npos;
  }

  inline std::string getInnerType(const std::string& type) {
    size_t first = type.find('[');
    size_t last = type.rfind(']');
    if (first == std::string::npos || last == std::string::npos ||
        last <= first)
      throw std::invalid_argument("Invalid list type format");
    return type.substr(0, first) +
           type.substr(first + 1, type.size() - first - 2);
  }

private:
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

  ClassAST* currentClass = nullptr;
  FunctionAST* currentFunction = nullptr;
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
