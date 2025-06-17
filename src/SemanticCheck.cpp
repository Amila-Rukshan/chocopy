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
    if (std::find(definedClassIds.begin(), definedClassIds.end(),
                  superClassId) == definedClassIds.end()) {
      errors.push_back(SemanticError(
          clazz->superClassLoc().line, clazz->superClassLoc().col,
          "Inherited class '" + superClassId + "' is not found\n"));
      errorFound = true;
    }
    if (!errorFound) {
      definedClassIds.push_back(classId);
      definedClasses[classId] = clazz.get();

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
  for (auto& globFunc : program.getFuncDefs()) {
    currentFunction = globFunc.get();
    globFunc->accept(*this);
    currentFunction = nullptr;
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
  definedFunctions[func.getId().str()] = &func;

  for (auto& arg : func.getArgs()) {
    arg->accept(*this);
    localVarToType[arg->getId().str()] = arg->getTypeInfo();
  }

  for (auto& localVar : func.getVarDefs()) {
    localVarToType[localVar->getTypedVar()->getId().str()] =
        localVar->getTypedVar()->getType()->getTypeName();
  }

  // class methods type checks
  if (currentClass != nullptr) {
    if (func.getArgs().empty()) {
      errors.push_back(
          SemanticError(func.loc().line, func.loc().col,
                        "Method '" + func.getId().str() + "' in class '" +
                            currentClass->getId().str() +
                            "' must have at least one parameter 'self'\n"));
    } else {
      auto firstArg = func.getArgs().front().get();
      if (firstArg->getId() != "self") {
        errors.push_back(
            SemanticError(firstArg->loc().line, firstArg->loc().col,
                          "First parameter of method '" + func.getId().str() +
                              "' in class '" + currentClass->getId().str() +
                              "' must be named 'self'\n"));
      }
      if (firstArg->getTypeInfo() != currentClass->getId().str()) {
        errors.push_back(SemanticError(
            firstArg->loc().line, firstArg->loc().col,
            "First parameter 'self' of method '" + func.getId().str() +
                "' must be of type of its class '" +
                currentClass->getId().str() + "'\n"));
      }
    }
  }

  for (auto& stmt : func.getBody()) {
    stmt->accept(*this);
  }
};

void SemanticCheckVisitor::visitLiteralNumber(
    const LiteralNumberAST& literalNumber) {
  literalNumber.setTypeInfo("int");
}

void SemanticCheckVisitor::visitLiteralTrue(const LiteralTrueAST& literalTrue) {
  literalTrue.setTypeInfo("bool");
}

void SemanticCheckVisitor::visitLiteralFalse(
    const LiteralFalseAST& literalFalse) {
  literalFalse.setTypeInfo("bool");
}

void SemanticCheckVisitor::visitLiteralString(
    const LiteralStringAST& literalString) {
  literalString.setTypeInfo("str");
}

void SemanticCheckVisitor::visitLiteralNone(const LiteralNoneAST& literalNone) {
  literalNone.setTypeInfo("<None>");
};

void SemanticCheckVisitor::visitCallExpr(const CallExprAST& callExpr) {
  for (const auto& arg : callExpr.getArgs()) {
    arg->accept(*this);
  }
  if (auto callee = llvm::dyn_cast<IdExprAST>(callExpr.getCallee())) {
    if (auto classPtr = definedClasses[callee->getId().str()]) {
      callExpr.setTypeInfo(classPtr->getId().str());
    } else if (auto funcPtr = definedFunctions[callee->getId().str()]) {
      callExpr.setTypeInfo(funcPtr->getReturnType()->getTypeName());
    }
  } else {
    // TODO: look for function in current class or global scope
  }
}

void SemanticCheckVisitor::visitIdExpr(const IdExprAST& idExpr) {
  if (currentClass == nullptr && currentFunction == nullptr) {
    if (globalVarToType.find(idExpr.getId().str()) != globalVarToType.end()) {
      idExpr.setTypeInfo(globalVarToType.at(idExpr.getId().str()));
    }
  } else if (currentClass != nullptr && currentFunction != nullptr) {
    if (idExpr.getId() == "self") {
      idExpr.setTypeInfo(currentClass->getId().str());
    } else if (localVarToType.find(idExpr.getId().str()) !=
               localVarToType.end()) {
      idExpr.setTypeInfo(localVarToType.at(idExpr.getId().str()));
    } else if (globalVarToType.find(idExpr.getId().str()) !=
               globalVarToType.end()) {
      idExpr.setTypeInfo(globalVarToType.at(idExpr.getId().str()));
    }
  } else if (currentClass == nullptr && currentFunction != nullptr) {
    if (localVarToType.find(idExpr.getId().str()) != localVarToType.end()) {
      auto tt = localVarToType.at(idExpr.getId().str());
      idExpr.setTypeInfo(localVarToType.at(idExpr.getId().str()));
    } else if (globalVarToType.find(idExpr.getId().str()) !=
               globalVarToType.end()) {
      idExpr.setTypeInfo(globalVarToType.at(idExpr.getId().str()));
    }
  }
}

void SemanticCheckVisitor::visitBinaryExpr(const BinaryExprAST& binaryExpr) {
  binaryExpr.getLhs()->accept(*this);
  binaryExpr.getRhs()->accept(*this);
  switch (binaryExpr.getOp()) {
  case TokenKind::kAttrAccessOp: {
    auto lhsType = binaryExpr.getLhs()->getTypeInfo();
    auto rhsId = llvm::dyn_cast<IdExprAST>(binaryExpr.getRhs());
    if (rhsId) {
      const VarDefAST* attr = lookupAttributeInHierarchy(
          definedClasses[lhsType], rhsId->getId().str());
      if (attr) {
        binaryExpr.setTypeInfo(attr->getTypedVar()->getType()->getTypeName());
      } else {
        errors.push_back(
            SemanticError(rhsId->loc().line, rhsId->loc().col,
                          "Unknown attribute: " + rhsId->getId().str() + "\n"));
      }
      return;
    }
    auto rhsCall = llvm::dyn_cast<CallExprAST>(binaryExpr.getRhs());
    if (std::find(definedClassIds.begin(), definedClassIds.end(), lhsType) !=
            definedClassIds.end() &&
        rhsCall) {
      auto calleeId = llvm::dyn_cast<IdExprAST>(rhsCall->getCallee());
      auto classPtr = definedClasses[lhsType];
      if (calleeId) {
        const FunctionAST* method =
            lookupMethodInHierarchy(classPtr, calleeId->getId().str());
        if (method) {
          binaryExpr.setTypeInfo(method->getReturnType()->getTypeName());
        } else {
          errors.push_back(SemanticError(
              calleeId->loc().line, calleeId->loc().col,
              "Unknown method: " + calleeId->getId().str() + "\n"));
        }
      }
      return;
    }
    return;
  }
  case TokenKind::kPlus: {
    auto lhsType = binaryExpr.getLhs()->getTypeInfo();
    auto rhsType = binaryExpr.getRhs()->getTypeInfo();
    if (lhsType == "str" && rhsType == "str") {
      binaryExpr.setTypeInfo("str");
    } else if (lhsType == "int" && rhsType == "int") {
      binaryExpr.setTypeInfo("int");
    } else {
      errors.push_back(
          SemanticError(binaryExpr.loc().line, binaryExpr.loc().col,
                        "Unsupported types for '+' operator: '" + lhsType +
                            "' and '" + rhsType + "'\n"));
    }
    return;
  }
  case TokenKind::kMinus: {
    auto lhsType = binaryExpr.getLhs()->getTypeInfo();
    auto rhsType = binaryExpr.getRhs()->getTypeInfo();
    if (lhsType == "int" && rhsType == "int") {
      binaryExpr.setTypeInfo("int");
    } else {
      errors.push_back(
          SemanticError(binaryExpr.loc().line, binaryExpr.loc().col,
                        "Unsupported types for '-' operator: '" + lhsType +
                            "' and '" + rhsType + "'\n"));
    }
    return;
  }
  case TokenKind::kMul: {
    auto lhsType = binaryExpr.getLhs()->getTypeInfo();
    auto rhsType = binaryExpr.getRhs()->getTypeInfo();
    if (lhsType == "int" && rhsType == "int") {
      binaryExpr.setTypeInfo("int");
    } else {
      errors.push_back(
          SemanticError(binaryExpr.loc().line, binaryExpr.loc().col,
                        "Unsupported types for '*' operator: '" + lhsType +
                            "' and '" + rhsType + "'\n"));
    }
    return;
  }
  case TokenKind::kIntDiv: {
    auto lhsType = binaryExpr.getLhs()->getTypeInfo();
    auto rhsType = binaryExpr.getRhs()->getTypeInfo();
    if (lhsType == "int" && rhsType == "int") {
      binaryExpr.setTypeInfo("int");
    } else {
      errors.push_back(
          SemanticError(binaryExpr.loc().line, binaryExpr.loc().col,
                        "Unsupported types for '//' operator: '" + lhsType +
                            "' and '" + rhsType + "'\n"));
    }
    auto lhsLiteral = llvm::dyn_cast<LiteralExprAST>(binaryExpr.getRhs());
    if (lhsLiteral) {
      if (auto lhsNumberLiteral =
              llvm::dyn_cast<LiteralNumberAST>(lhsLiteral->getLiteral())) {
        if (lhsNumberLiteral->getNumber() == 0) {
          errors.push_back(
              SemanticError(binaryExpr.loc().line, binaryExpr.loc().col,
                            "Division by zero in '//' operator\n"));
        }
      }
    }
    return;
  }
  case TokenKind::kMod: {
    auto lhsType = binaryExpr.getLhs()->getTypeInfo();
    auto rhsType = binaryExpr.getRhs()->getTypeInfo();
    if (lhsType == "int" && rhsType == "int") {
      binaryExpr.setTypeInfo("int");
    } else {
      errors.push_back(
          SemanticError(binaryExpr.loc().line, binaryExpr.loc().col,
                        "Unsupported types for '%' operator: '" + lhsType +
                            "' and '" + rhsType + "'\n"));
    }
    auto lhsLiteral = llvm::dyn_cast<LiteralExprAST>(binaryExpr.getRhs());
    if (lhsLiteral) {
      if (auto lhsNumberLiteral =
              llvm::dyn_cast<LiteralNumberAST>(lhsLiteral->getLiteral())) {
        if (lhsNumberLiteral->getNumber() == 0) {
          errors.push_back(SemanticError(binaryExpr.loc().line,
                                         binaryExpr.loc().col,
                                         "Division by zero in '%' operator\n"));
        }
      }
    }
    return;
  }
  case TokenKind::kEqual: {
    auto lhsType = binaryExpr.getLhs()->getTypeInfo();
    auto rhsType = binaryExpr.getRhs()->getTypeInfo();
    if (lhsType == rhsType &&
        (lhsType == "bool" || lhsType == "int" || lhsType == "str")) {
      binaryExpr.setTypeInfo("bool");
    } else {
      errors.push_back(
          SemanticError(binaryExpr.loc().line, binaryExpr.loc().col,
                        "Unsupported types for '==' operator: '" + lhsType +
                            "' and '" + rhsType + "'\n"));
    }
    return;
  }
  case TokenKind::kInEqual: {
    auto lhsType = binaryExpr.getLhs()->getTypeInfo();
    auto rhsType = binaryExpr.getRhs()->getTypeInfo();
    if (lhsType == rhsType &&
        (lhsType == "bool" || lhsType == "int" || lhsType == "str")) {
      binaryExpr.setTypeInfo("bool");
    } else {
      errors.push_back(
          SemanticError(binaryExpr.loc().line, binaryExpr.loc().col,
                        "Unsupported types for '!=' operator: '" + lhsType +
                            "' and '" + rhsType + "'\n"));
    }
    return;
  }
  case TokenKind::kLessThan:
  case TokenKind::kGreaterThan:
  case TokenKind::kLessThanOrEqual:
  case TokenKind::kGreaterThanOrEqual: {
    auto lhsType = binaryExpr.getLhs()->getTypeInfo();
    auto rhsType = binaryExpr.getRhs()->getTypeInfo();
    if (lhsType == "int" && rhsType == "int") {
      binaryExpr.setTypeInfo("bool");
    } else {
      errors.push_back(
          SemanticError(binaryExpr.loc().line, binaryExpr.loc().col,
                        "Unsupported types for comparison operator: '" +
                            lhsType + "' and '" + rhsType + "'\n"));
    }
    return;
  }
  case TokenKind::k_and:
  case TokenKind::k_or: {
    auto lhsType = binaryExpr.getLhs()->getTypeInfo();
    auto rhsType = binaryExpr.getRhs()->getTypeInfo();
    if (lhsType == "bool" && rhsType == "bool") {
      binaryExpr.setTypeInfo("bool");
    } else {
      errors.push_back(SemanticError(
          binaryExpr.loc().line, binaryExpr.loc().col,
          "Unsupported types for '" + tokenKindToString(binaryExpr.getOp()) +
              "' operator: '" + lhsType + "' and '" + rhsType + "'\n"));
    }
    return;
  }
  case TokenKind::k_is: {
    auto lhsType = binaryExpr.getLhs()->getTypeInfo();
    auto rhsType = binaryExpr.getRhs()->getTypeInfo();
    if (!isPrimitiveType(lhsType) && !isPrimitiveType(lhsType)) {
      binaryExpr.setTypeInfo("bool");
    } else {
      errors.push_back(
          SemanticError(binaryExpr.loc().line, binaryExpr.loc().col,
                        "Unsupported types for 'is' operator: '" + lhsType +
                            "' and '" + rhsType + "'\n"));
    }
    return;
  }
  }
}

void SemanticCheckVisitor::visitUnaryExpr(const UnaryExprAST& unaryExpr) {
  unaryExpr.getExpr()->accept(*this);
  auto exprType = unaryExpr.getExpr()->getTypeInfo();
  switch (unaryExpr.getOp()) {
  case TokenKind::k_not: {
    if (exprType == "bool") {
      unaryExpr.setTypeInfo("bool");
    } else {
      errors.push_back(SemanticError(unaryExpr.loc().line, unaryExpr.loc().col,
                                     "Unsupported type for 'not' operator: '" +
                                         exprType + "'\n"));
    }
    return;
  }
  case TokenKind::kMinus: {
    if (exprType == "int") {
      unaryExpr.setTypeInfo("int");
    } else {
      errors.push_back(SemanticError(unaryExpr.loc().line, unaryExpr.loc().col,
                                     "Unsupported type for '-' operator: '" +
                                         exprType + "'\n"));
    }
    return;
  }
  default: {
    errors.push_back(SemanticError(unaryExpr.loc().line, unaryExpr.loc().col,
                                   "Unknown unary operator: '" +
                                       tokenKindToString(unaryExpr.getOp()) +
                                       "'\n"));
    return;
  }
  }
}

void SemanticCheckVisitor::visitIfElseExpr(const IfElseExprAST& ifElseExpr) {
  ifElseExpr.getCondition()->accept(*this);
  auto conditionType = ifElseExpr.getCondition()->getTypeInfo();
  if (conditionType != "bool") {
    errors.push_back(SemanticError(ifElseExpr.loc().line, ifElseExpr.loc().col,
                                   "Condition must be of type 'bool', found '" +
                                       conditionType + "'\n"));
  }
  ifElseExpr.getIfBody()->accept(*this);
  if (ifElseExpr.getElseBody()) {
    ifElseExpr.getElseBody()->accept(*this);
  }
  auto ifBodyType = ifElseExpr.getIfBody()->getTypeInfo();
  auto elseBodyType = ifElseExpr.getElseBody()->getTypeInfo();

  if ((isPrimitiveType(ifBodyType) || isPrimitiveType(elseBodyType)) &&
      ifBodyType != elseBodyType) {
    errors.push_back(SemanticError(
        ifElseExpr.loc().line, ifElseExpr.loc().col,
        "If and else bodies must have the same primitive type, found '" +
            ifBodyType + "' and '" + elseBodyType + "'\n"));
  }

  auto type = typeUnion(ifBodyType, elseBodyType);
  ifElseExpr.setTypeInfo(type);
}

std::string SemanticCheckVisitor::typeUnion(const std::string& lhsType,
                                            const std::string& rhsType) {
  if (lhsType == "int" && rhsType == "int")
    return "int";
  else if (lhsType == "bool" && rhsType == "bool")
    return "bool";
  else if (lhsType == "str" && rhsType == "str")
    return "str";

  // Find the least common ancestor in the class hierarchy
  ClassAST* lhsClass = definedClasses[lhsType];
  ClassAST* rhsClass = definedClasses[rhsType];

  // Traverse up the hierarchy to find the least common ancestor
  while (lhsClass && rhsClass) {
    if (lhsClass->getId() == rhsClass->getId()) {
      return lhsClass->getId().str();
    }
    lhsClass = const_cast<ClassAST*>(lhsClass->getParentClass());
    rhsClass = const_cast<ClassAST*>(rhsClass->getParentClass());
  }
  return "object";
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
    const SimpleStmtAssignAST& simpleStmtAssign) {
  for (const auto& target : simpleStmtAssign.getTargets()) {
    target->accept(*this);
  }
  simpleStmtAssign.getRhs()->accept(*this);
  // Check if the right-hand side expression is compatible with the
  // left-hand side targets
  auto rhsType = simpleStmtAssign.getRhs()->getTypeInfo();
  for (const auto& target : simpleStmtAssign.getTargets()) {
    auto targetType = target->getTypeInfo();
    if (isPrimitiveType(targetType) && isPrimitiveType(rhsType) &&
        targetType != rhsType) {
      errors.push_back(SemanticError(target->loc().line, target->loc().col,
                                     "Type mismatch: cannot assign '" +
                                         rhsType + "' to '" + targetType +
                                         "'\n"));
    } else {
      if (!isSubTypeOf(rhsType, targetType)) {
        errors.push_back(SemanticError(target->loc().line, target->loc().col,
                                       "Type mismatch: cannot assign '" +
                                           rhsType + "' to '" + targetType +
                                           "'\n"));
      }
    }
  }
}

void SemanticCheckVisitor::visitSimpleStmtExpr(
    const SimpleStmtExprAST& simpleStmtExpr) {}

void SemanticCheckVisitor::visitStmtIf(const StmtIfAST& stmtIf) {
  stmtIf.getCondition()->accept(*this);
  auto conditionType = stmtIf.getCondition()->getTypeInfo();
  if (conditionType != "bool") {
    errors.push_back(SemanticError(
        stmtIf.getCondition()->loc().line, stmtIf.getCondition()->loc().col,
        "Condition must be of type 'bool', found '" + conditionType + "'\n"));
  }
  for (const auto& ifBodyStmt : stmtIf.getBody()) {
    ifBodyStmt->accept(*this);
  }
  for (const auto& elIfBlock : stmtIf.getElifs()) {
    elIfBlock->accept(*this);
  }
  for (const auto& elseBodyStmt : stmtIf.getElseBody()) {
    elseBodyStmt->accept(*this);
  }
}

void SemanticCheckVisitor::visitStmtWhile(const StmtWhileAST& stmtWhile) {
  stmtWhile.getCondition()->accept(*this);
  auto conditionType = stmtWhile.getCondition()->getTypeInfo();
  if (conditionType != "bool") {
    errors.push_back(SemanticError(stmtWhile.getCondition()->loc().line,
                                   stmtWhile.getCondition()->loc().col,
                                   "Condition must be of type 'bool', found '" +
                                       conditionType + "'\n"));
  }
  for (const auto& whileBodyStmt : stmtWhile.getBody()) {
    whileBodyStmt->accept(*this);
  }
}

void SemanticCheckVisitor::visitSimpleStmtReturn(
    const SimpleStmtReturnAST& simpleStmtReturn) {
  if (simpleStmtReturn.getExpr())
    simpleStmtReturn.getExpr()->accept(*this);
}

} // namespace chocopy
