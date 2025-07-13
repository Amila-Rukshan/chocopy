#include <algorithm>
#include <iostream>
#include <stack>

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
  for (auto& varDef : program.getVarDefs()) {
    varDef->accept(*this);
  }
  for (auto& globFunc : program.getFuncDefs()) {
    functionStack.push(globFunc.get());
    globFunc->accept(*this);
    functionStack.pop();
  }
  for (auto& stmt : program.getStmts()) {
    stmt->accept(*this);
  }
}

void SemanticCheckVisitor::visitClass(const ClassAST& clazz) {
  for (auto& method : clazz.getMethodDefs()) {
    functionStack.push(method.get());
    method->accept(*this);
    functionStack.pop();
  }
}

void SemanticCheckVisitor::visitFunction(const FunctionAST& func) {
  if (!func.isNestedFunc()) {
    definedFunctions[currentClass != nullptr ? currentClass->getId().str() +
                                                   "-" + func.getId().str()
                                             : func.getId().str()] = &func;
  } else {
    std::string fullyQualifiedName = func.getId().str();
    const FunctionAST* parentFuncPtr = func.getParentFunc();
    while (parentFuncPtr != nullptr) {
      fullyQualifiedName =
          parentFuncPtr->getId().str() + "-" + fullyQualifiedName;
      parentFuncPtr = parentFuncPtr->getParentFunc();
    }
    if (currentClass) {
      fullyQualifiedName =
          currentClass->getId().str() + "-" + fullyQualifiedName;
    }
    definedFunctions[fullyQualifiedName] = &func;
  }

  for (auto& arg : func.getArgs()) {
    arg->accept(*this);
    localVarToType[arg->getId().str()] = arg->getTypeInfo();
  }

  for (auto& localVar : func.getVarDefs()) {
    localVarToType[localVar->getTypedVar()->getId().str()] =
        localVar->getTypedVar()->getType()->getTypeName();
  }

  for (auto& nestedFunc : func.getFuncDefs()) {
    functionStack.push(nestedFunc.get());
    nestedFunc->accept(*this);
    functionStack.pop();
  }

  // class methods type checks
  if (currentClass != nullptr && !func.isNestedFunc()) {
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

void SemanticCheckVisitor::visitListLiteralExpr(
    const ListLiteralExprAST& listLiteralExpr) {
  size_t maxDepth = 1;
  size_t elementCount = 0;
  std::string listLiteralType = "<Empty>";
  std::stack<std::pair<const ListLiteralExprAST*, size_t>> stack;
  stack.push({&listLiteralExpr, 1});
  while (!stack.empty()) {
    auto [top, depth] = stack.top();
    stack.pop();
    for (auto& element : top->getElements()) {
      if (auto* inner = llvm::dyn_cast<ListLiteralExprAST>(element.get())) {
        inner->accept(*this);
        stack.push({inner, depth + 1});
        if (maxDepth < depth + 1) {
          maxDepth = depth + 1;
        }
      } else if (llvm::isa<LiteralExprAST, CallExprAST>(element.get())) {
        if (maxDepth != depth) {
          errors.push_back(SemanticError(
              element->loc().line, element->loc().col,
              "List elements must have the same depth in a list literal\n"));
        }
        elementCount++;
        element->accept(*this);
        listLiteralType = typeUnion(listLiteralType, element->getTypeInfo());
      }
    }
  }
  const_cast<ListLiteralExprAST&>(listLiteralExpr).setListDimension(maxDepth);
  listLiteralExpr.setTypeInfo(listLiteralType + std::string(maxDepth, '[') +
                              std::string(maxDepth, ']'));
}

std::string SemanticCheckVisitor::getFQN(const CallExprAST& callExpr) {
  if (auto callee = llvm::dyn_cast<IdExprAST>(callExpr.getCallee())) {
    if (callExpr.getSelfExpr())
      return callExpr.getSelfExpr()->getTypeInfo() + "-" +
             callee->getId().str();
    else {
      std::string fullyQualifiedName = callee->getId().str();
      const FunctionAST* parentFunction = currentFunction();
      while (parentFunction) {
        if (fullyQualifiedName != parentFunction->getId().str()) {
          fullyQualifiedName =
              parentFunction->getId().str() + "-" + fullyQualifiedName;
        }
        parentFunction = parentFunction->getParentFunc();
      }
      if (currentClass) {
        fullyQualifiedName =
            currentClass->getId().str() + "-" + fullyQualifiedName;
      }
      return fullyQualifiedName;
    }
  }
}

void SemanticCheckVisitor::visitCallExpr(const CallExprAST& callExpr) {
  for (const auto& arg : callExpr.getArgs()) {
    arg->accept(*this);
  }
  if (auto callee = llvm::dyn_cast<IdExprAST>(callExpr.getCallee())) {
    if (auto classPtr = definedClasses[callee->getId().str()]) {
      callExpr.setTypeInfo(classPtr->getId().str());
    } else if (auto funcPtr = definedFunctions[getFQN(callExpr)]) {
      if (callee->getId().str() == "print" &&
          callExpr.getSelfExpr() == nullptr) {
        if (callExpr.getArgs().empty()) {
          errors.push_back(SemanticError(
              callee->loc().line, callee->loc().col,
              "Function 'print' expects at least one argument\n"));
        } else {
          for (const auto& arg : callExpr.getArgs()) {
            if (arg->getTypeInfo() != "str" && arg->getTypeInfo() != "int" &&
                arg->getTypeInfo() != "bool") {
              errors.push_back(SemanticError(arg->loc().line, arg->loc().col,
                                             "Argument of 'print' must be of "
                                             "type 'str', 'int', or 'bool'\n"));
            }
          }
        }
        callExpr.setTypeInfo("None");
        return;
      } else if (funcPtr->getArgs().size() !=
                 (callExpr.getArgs().size() + (callExpr.getSelfExpr() ? 1
                                               : funcPtr->isNestedFunc()
                                                   ? 1
                                                   : 0))) {
        errors.push_back(SemanticError(
            callee->loc().line, callee->loc().col,
            "Function '" + callee->getId().str() + "' expects " +
                std::to_string(funcPtr->getArgs().size()) +
                " arguments, but got " +
                std::to_string(callExpr.getArgs().size()) + "\n"));
      } else {
        // Hanlde dispatch call / nested func call type checking
        if (callExpr.getSelfExpr() || funcPtr->isNestedFunc()) {
          if (callExpr.getSelfExpr()) {
            if (!isSubTypeOf(callExpr.getSelfExpr()->getTypeInfo(),
                             funcPtr->getArgs()[0]->getType()->getTypeName())) {
              errors.push_back(SemanticError(
                  callExpr.getSelfExpr()->loc().line,
                  callExpr.getSelfExpr()->loc().col,
                  "First argument of function '" + callee->getId().str() +
                      "' must be of type '" +
                      funcPtr->getArgs()[0]->getType()->getTypeName() +
                      "', but got '" + callExpr.getSelfExpr()->getTypeInfo() +
                      "'\n"));
            }
          }
          // Common args type check
          for (size_t argIndex = 1; argIndex < funcPtr->getArgs().size();
               ++argIndex) {
            auto argType = callExpr.getArgs()[argIndex - 1]->getTypeInfo();
            auto expectedType =
                funcPtr->getArgs()[argIndex]->getType()->getTypeName();
            if (!isSubTypeOf(argType, expectedType)) {
              errors.push_back(
                  SemanticError(callExpr.loc().line, callExpr.loc().col,
                                "Argument " + std::to_string(argIndex + 1) +
                                    " of function '" + callee->getId().str() +
                                    "' must be of type '" + expectedType +
                                    "', but got '" + argType + "'\n"));
            }
          }
        } else {
          for (size_t argIndex = 0; argIndex < funcPtr->getArgs().size();
               ++argIndex) {
            auto argType = callExpr.getArgs()[argIndex]->getTypeInfo();
            auto expectedType =
                funcPtr->getArgs()[argIndex]->getType()->getTypeName();
            if (!isSubTypeOf(argType, expectedType)) {
              errors.push_back(
                  SemanticError(callExpr.loc().line, callExpr.loc().col,
                                "Argument " + std::to_string(argIndex + 1) +
                                    " of function '" + callee->getId().str() +
                                    "' must be of type '" + expectedType +
                                    "', but got '" + argType + "'\n"));
            }
          }
        }
      }
      callExpr.setTypeInfo(funcPtr->getReturnType()->getTypeName());
    } else if (callee->getId() == "len") {
      callExpr.setTypeInfo("int");
    }
  } else {
    // TODO: look for function in current class or global scope
  }
}

void SemanticCheckVisitor::visitIdExpr(const IdExprAST& idExpr) {
  if (currentClass == nullptr && currentFunction() == nullptr) {
    if (localVarToType.find(idExpr.getId().str()) != localVarToType.end()) {
      idExpr.setTypeInfo(localVarToType.at(idExpr.getId().str()));
    } else if (globalVarToType.find(idExpr.getId().str()) !=
               globalVarToType.end()) {
      idExpr.setTypeInfo(globalVarToType.at(idExpr.getId().str()));
    }
  } else if (currentClass != nullptr && currentFunction() != nullptr) {
    if (idExpr.getId() == "self") {
      idExpr.setTypeInfo(currentClass->getId().str());
    } else if (localVarToType.find(idExpr.getId().str()) !=
               localVarToType.end()) {
      idExpr.setTypeInfo(localVarToType.at(idExpr.getId().str()));
    } else if (globalVarToType.find(idExpr.getId().str()) !=
               globalVarToType.end()) {
      idExpr.setTypeInfo(globalVarToType.at(idExpr.getId().str()));
    }
  } else if (currentClass == nullptr && currentFunction() != nullptr) {
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
  // set dispatch and type info
  if (binaryExpr.getOp() == TokenKind::kAttrAccessOp) {
    if (auto rhsCall = const_cast<CallExprAST*>(
            llvm::dyn_cast<CallExprAST>(binaryExpr.getRhs()))) {
      rhsCall->setSelfExpr(binaryExpr.getLhs());
    }
  }
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
    } else if (isListType(lhsType) && isListType(rhsType) &&
               lhsType == rhsType) {
      binaryExpr.setTypeInfo(lhsType);
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
  case TokenKind::kIndexAccessOp: {
    auto lhsType = binaryExpr.getLhs()->getTypeInfo();
    auto rhsType = binaryExpr.getRhs()->getTypeInfo();
    if (lhsType == "str" && rhsType == "int") {
      binaryExpr.setTypeInfo("str");
    } else if (isListType(lhsType) && rhsType == "int") {
      auto outputType = getInnerType(lhsType);
      if (isListType(outputType)) {
        const_cast<BinaryExprAST&>(binaryExpr)
            .setAccessKind(AccessKind::ListAccess);
      }
      binaryExpr.setTypeInfo(outputType);
    } else {
      errors.push_back(SemanticError(
          binaryExpr.loc().line, binaryExpr.loc().col,
          "Unsupported types for '" + tokenKindToString(binaryExpr.getOp()) +
              "' operator: '" + lhsType + "' and '" + rhsType + "'\n"));
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
  if (lhsType == "<Empty>")
    return rhsType;
  if (rhsType == "<Empty>")
    return lhsType;
  if (lhsType == "UNTYPED")
    return rhsType;
  else if (rhsType == "UNTYPED")
    return lhsType;
  else if (lhsType == "int" && rhsType == "int")
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
  if (currentClass == nullptr && currentFunction() == nullptr) {
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

  if (auto listTypeAST = llvm::dyn_cast<ListTypeAST>(typedVar.getType())) {
    if (isDefinedType(listTypeAST->getType()->getTypeName())) {
      typedVar.setTypeInfo(listTypeAST->getTypeName());
      return;
    } else {
      errors.push_back(
          SemanticError(listTypeAST->loc().line, listTypeAST->loc().col,
                        "Undefined type for list type: " +
                            listTypeAST->getType()->getTypeName() + "\n"));
    }
  }
}

void SemanticCheckVisitor::visitSimpleStmtAssign(
    const SimpleStmtAssignAST& simpleStmtAssign) {
  for (const auto& target : simpleStmtAssign.getTargets()) {
    target->accept(*this);
    if (auto binaryExpr = llvm::dyn_cast<BinaryExprAST>(target.get())) {
      if (binaryExpr->getOp() == TokenKind::kIndexAccessOp) {
        target->setAccessKind(AccessKind::Write);
      }
    }
  }
  simpleStmtAssign.getRhs()->accept(*this);
  // Check if the right-hand side expression is compatible with the
  // left-hand side targets
  auto rhsType = simpleStmtAssign.getRhs()->getTypeInfo();
  for (const auto& target : simpleStmtAssign.getTargets()) {
    auto targetType = target->getTypeInfo();
    if (!isSubTypeOf(rhsType, targetType)) {
      errors.push_back(SemanticError(target->loc().line, target->loc().col,
                                     "Type mismatch: cannot assign '" +
                                         rhsType + "' to '" + targetType +
                                         "'\n"));
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

void SemanticCheckVisitor::visitStmtFor(const StmtForAST& stmtFor) {
  stmtFor.getExpr()->accept(*this);
  auto type = stmtFor.getExpr()->getTypeInfo();
  if (type != "str" && !isListType(type)) {
    errors.push_back(SemanticError(
        stmtFor.getExpr()->loc().line, stmtFor.getExpr()->loc().col,
        "For loop can only enumerate 'list' or 'str' values\n"));
  }
  if (type == "str") {
    stmtFor.getTypedVar()->setTypeInfo("str");
  } else if (isListType(type)) {
    stmtFor.getTypedVar()->setTypeInfo(getInnerType(type));
  }
  localVarToType[stmtFor.getTypedVar()->getId().str()] =
      stmtFor.getTypedVar()->getTypeInfo();
  for (const auto& forBodyStmt : stmtFor.getBody()) {
    forBodyStmt->accept(*this);
  }
}

void SemanticCheckVisitor::visitSimpleStmtReturn(
    const SimpleStmtReturnAST& simpleStmtReturn) {
  if (simpleStmtReturn.getExpr())
    simpleStmtReturn.getExpr()->accept(*this);
  auto retType = simpleStmtReturn.getExpr()
                     ? simpleStmtReturn.getExpr()->getTypeInfo()
                     : "<None>";
  auto funcReturnType = currentFunction()
                            ? currentFunction()->getReturnType()->getTypeName()
                            : "<None>";
  if (isPrimitiveType(funcReturnType) && isPrimitiveType(retType) &&
      funcReturnType != retType) {
    errors.push_back(
        SemanticError(simpleStmtReturn.loc().line, simpleStmtReturn.loc().col,
                      "Return type mismatch: expected '" + funcReturnType +
                          "', found '" + retType + "'\n"));
  } else if (!isSubTypeOf(retType, funcReturnType)) {
    errors.push_back(
        SemanticError(simpleStmtReturn.loc().line, simpleStmtReturn.loc().col,
                      "Return type mismatch: expected '" + funcReturnType +
                          "', found '" + retType + "'\n"));
  }
}

bool SemanticCheckVisitor::isSubTypeOf(const std::string& subType,
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

const FunctionAST* SemanticCheckVisitor::currentFunction() const {
  return functionStack.empty() ? nullptr : functionStack.top();
}

} // namespace chocopy
