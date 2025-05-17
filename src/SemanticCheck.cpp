#include <algorithm>

#include "SemanticCheck.h"
#include "llvm/ADT/Twine.h"
#include "llvm/ADT/TypeSwitch.h"

namespace chocopy {

SemanticCheckVisitor::SemanticCheckVisitor() {}

SemanticCheckVisitor::~SemanticCheckVisitor() {}

std::vector<SemanticError>
SemanticCheckVisitor::check(const ProgramAST& program) {
  auto errors = checkInheritance(program);
  if (!errors.empty())
    return errors;
  program.accept(*this);
  return this->errors;
}

std::vector<SemanticError>
SemanticCheckVisitor::checkInheritance(const ProgramAST& program) {
  for (auto& clazz : program.getClassDefs()) {
    const std::string superClassId = clazz->getSuperClassId().str();
    const std::string classId = clazz->getId().str();
    bool errorFound = false;
    if (classId == "object" || classId == "str" || classId == "int" ||
        classId == "bool") {
      errors.push_back(
          SemanticError(clazz->classLoc().line, clazz->classLoc().col,
                        "Cannot create a class named '" + classId +
                            "' as it conflicts with a built-in type\n"));
      errorFound = true;
    }
    if (superClassId == "str" || superClassId == "int" ||
        superClassId == "bool") {
      errors.push_back(SemanticError(
          clazz->superClassLoc().line, clazz->superClassLoc().col,
          "Cannot inherit from built-in type '" + superClassId + "'\n"));
      errorFound = true;
    }
    if (classId == superClassId) {
      errors.push_back(SemanticError(
          clazz->superClassLoc().line, clazz->superClassLoc().col,
          "class '" + classId + "' cannot inherit from itself\n"));
      errorFound = true;
    }
    if (std::find(definedClassIds.begin(), definedClassIds.end(), classId) !=
        definedClassIds.end()) {
      errors.push_back(
          SemanticError(clazz->classLoc().line, clazz->classLoc().col,
                        "class '" + classId + "' is already defined\n"));
      errorFound = true;
    }
    if (!errorFound) {
      definedClassIds.push_back(classId);

      auto superClass = program.GetClassPtr(superClassId);
      clazz->setParentClass(superClass);
      superClass->AddChildClass(clazz.get());
    }
  }
  return errors;
}

void SemanticCheckVisitor::visitProgram(const ProgramAST& program) {
  for (auto& clazz : program.getClassDefs()) {
    currentClass = clazz.get();
    clazz->accept(*this);
    currentClass = nullptr;
  }
  for (auto& varDef : program.getVarDefs()) {
    varDef->accept(*this);
  }
  for (auto& stmt : program.getStmts()) {
    stmt->accept(*this);
  }
}

void SemanticCheckVisitor::visitClass(const ClassAST& clazz) {
  for (auto& method : clazz.getMethodDefs()) {
    currentFunction = method.get();
    method->accept(*this);
    currentFunction = nullptr;
  }
}

void SemanticCheckVisitor::visitFunction(const FunctionAST& func) {
  for (auto& arg : func.getArgs()) {
    arg->accept(*this);
  }
  for (auto& stmt : func.getBody()) {
    stmt->accept(*this);
  }
};

void SemanticCheckVisitor::visitLiteralNumber(
    const LiteralNumberAST& literalNumber) {}

void SemanticCheckVisitor::visitLiteralTrue(const LiteralTrueAST& literalTrue) {
}

void SemanticCheckVisitor::visitLiteralFalse(
    const LiteralFalseAST& literalFalse) {}

void SemanticCheckVisitor::visitLiteralString(
    const LiteralStringAST& literalString) {}

void SemanticCheckVisitor::visitLiteralNone(const LiteralNoneAST& literalNone) {
};

void SemanticCheckVisitor::visitCallExpr(const CallExprAST& callExpr) {
  for (const auto& arg : callExpr.getArgs()) {
    arg->accept(*this);
  }
}

void SemanticCheckVisitor::visitIdExpr(const IdExprAST& idExpr) {
  if (currentClass == nullptr && currentFunction == nullptr) {
    idExpr.setTypeInfo(globalVarToType.at(idExpr.getId().str()));
  } else if (currentClass != nullptr && currentFunction != nullptr) {
    if (idExpr.getId() == "self") {
      idExpr.setTypeInfo(currentClass->getId().str());
    }
  }
}

void SemanticCheckVisitor::visitBinaryExpr(const BinaryExprAST& binaryExpr) {
  binaryExpr.getLhs()->accept(*this);
  binaryExpr.getRhs()->accept(*this);
}

void SemanticCheckVisitor::visitVarDef(const VarDefAST& varDef) {
  varDef.getTypedVar()->accept(*this);
  auto type = varDef.getTypedVar()->getTypeInfo();
  if (type == "str") {
    auto stringLiteral = llvm::dyn_cast<LiteralStringAST>(varDef.getLiteral());
    if (stringLiteral == nullptr) {
      errors.push_back(SemanticError(
          varDef.getLiteral()->loc().line, varDef.getLiteral()->loc().col,
          "Variable '" + varDef.getTypedVar()->getId().str() +
              "' of type 'str' must be initialized with a string literal\n"));
    }
  } else if (type == "int") {
    auto numberLiteral = llvm::dyn_cast<LiteralNumberAST>(varDef.getLiteral());
    if (numberLiteral == nullptr) {
      errors.push_back(SemanticError(
          varDef.getLiteral()->loc().line, varDef.getLiteral()->loc().col,
          "Variable '" + varDef.getTypedVar()->getId().str() +
              "' of type 'int' must be initialized with a number literal\n"));
    }
  } else if (type == "bool") {
    auto falseLiteral = llvm::dyn_cast<LiteralFalseAST>(varDef.getLiteral());
    auto trueLiteral = llvm::dyn_cast<LiteralTrueAST>(varDef.getLiteral());
    if (falseLiteral == nullptr && trueLiteral == nullptr) {
      errors.push_back(SemanticError(
          varDef.getLiteral()->loc().line, varDef.getLiteral()->loc().col,
          "Variable '" + varDef.getTypedVar()->getId().str() +
              "' of type 'bool' must be initialized with 'True' or 'False' "
              "literal values\n"));
    }
  } else {
    auto noneLiteral = llvm::dyn_cast<LiteralNoneAST>(varDef.getLiteral());
    if (noneLiteral == nullptr) {
      errors.push_back(SemanticError(
          varDef.getLiteral()->loc().line, varDef.getLiteral()->loc().col,
          "Variable '" + varDef.getTypedVar()->getId().str() + "' of type '" +
              varDef.getTypedVar()->getTypeInfo() +
              "' must be initialized with 'None' literal\n"));
    }
  }
  if (currentClass == nullptr && currentFunction == nullptr) {
    globalVarToType[varDef.getTypedVar()->getId().str()] =
        varDef.getTypedVar()->getTypeInfo();
  }
}

void SemanticCheckVisitor::visitTypedVar(const TypedVarAST& typedVar) {
  if (auto idTypeAST = llvm::dyn_cast<IdTypeAST>(typedVar.getType())) {
    if (isDefinedType(idTypeAST->getId())) {
      typedVar.setTypeInfo(idTypeAST->getId().str());
      return;
    } else {
      errors.push_back(
          SemanticError(idTypeAST->loc().line, idTypeAST->loc().col,
                        "Undefined type: " + idTypeAST->getId().str() + "\n"));
    }
  }

  if (auto idStringTypeAST =
          llvm::dyn_cast<IdStringTypeAST>(typedVar.getType())) {
    if (isDefinedType(idStringTypeAST->getId())) {
      typedVar.setTypeInfo(idStringTypeAST->getId().str());
      return;
    } else {
      errors.push_back(SemanticError(
          idStringTypeAST->loc().line, idStringTypeAST->loc().col,
          "Undefined type: " + idStringTypeAST->getId().str() + "\n"));
    }
  }
  auto listTypeAST = llvm::dyn_cast<ListTypeAST>(typedVar.getType());
  assert(listTypeAST == nullptr && "List type is not implemented yet");
}

void SemanticCheckVisitor::visitSimpleStmtAssign(
    const SimpleStmtAssignAST& simpleStmtAssign) {}

void SemanticCheckVisitor::visitSimpleStmtExpr(
    const SimpleStmtExprAST& simpleStmtExpr) {}

void SemanticCheckVisitor::visitSimpleStmtReturn(
    const SimpleStmtReturnAST& simpleStmtReturn) {
  simpleStmtReturn.getExpr()->accept(*this);
}

} // namespace chocopy
