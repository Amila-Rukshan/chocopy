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
  // Register every function and method up front so that calls resolve
  // regardless of the order of definitions
  for (auto& clazz : program.getClassDefs()) {
    for (auto& method : clazz->getMethodDefs()) {
      registerFunction(*method, clazz->getId().str());
    }
  }
  for (auto& globFunc : program.getFuncDefs()) {
    registerFunction(*globFunc, "");
  }
  // Global variables are visible in every class and function body
  for (auto& varDef : program.getVarDefs()) {
    varDef->accept(*this);
  }
  checkGlobalNames(program);
  for (auto& clazz : program.getClassDefs()) {
    currentClass = clazz.get();
    clazz->accept(*this);
    currentClass = nullptr;
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

void SemanticCheckVisitor::registerFunction(const FunctionAST& func,
                                            const std::string& prefix) {
  std::string fullyQualifiedName =
      prefix.empty() ? func.getId().str() : prefix + "-" + func.getId().str();
  definedFunctions[fullyQualifiedName] = &func;
  for (auto& nestedFunc : func.getFuncDefs()) {
    registerFunction(*nestedFunc, fullyQualifiedName);
  }
}

void SemanticCheckVisitor::checkGlobalNames(const ProgramAST& program) {
  std::unordered_set<std::string> globalNames;
  auto declare = [&](const std::string& name, const Location& loc) {
    if (isDefinedType(name)) {
      errors.push_back(SemanticError(
          loc.line, loc.col, "Cannot shadow class name: " + name + "\n"));
    } else if (!globalNames.insert(name).second) {
      errors.push_back(SemanticError(
          loc.line, loc.col,
          "Duplicate declaration of identifier in same scope: " + name + "\n"));
    }
  };
  for (auto& varDef : program.getVarDefs()) {
    declare(varDef->getTypedVar()->getId().str(), varDef->getTypedVar()->loc());
  }
  for (auto& globFunc : program.getFuncDefs()) {
    declare(globFunc->getId().str(), globFunc->loc());
  }
}

void SemanticCheckVisitor::visitClass(const ClassAST& clazz) {
  const ClassAST* parentClass = clazz.getParentClass();
  std::unordered_set<std::string> memberNames;

  // [attr-init]
  for (auto& attr : clazz.getVarDefs()) {
    attr->accept(*this);
    const std::string attrId = attr->getTypedVar()->getId().str();
    const Location& loc = attr->getTypedVar()->loc();
    if (!memberNames.insert(attrId).second) {
      errors.push_back(SemanticError(
          loc.line, loc.col,
          "Duplicate declaration of identifier in same scope: " + attrId +
              "\n"));
    } else if (lookupAttributeInHierarchy(parentClass, attrId) ||
               lookupMethodInHierarchy(parentClass, attrId)) {
      errors.push_back(SemanticError(
          loc.line, loc.col, "Cannot re-define attribute: " + attrId + "\n"));
    }
  }

  for (auto& method : clazz.getMethodDefs()) {
    const std::string methodId = method->getId().str();
    const Location& loc = method->loc();
    if (!memberNames.insert(methodId).second) {
      errors.push_back(SemanticError(
          loc.line, loc.col,
          "Duplicate declaration of identifier in same scope: " + methodId +
              "\n"));
      continue;
    }
    if (lookupAttributeInHierarchy(parentClass, methodId)) {
      errors.push_back(SemanticError(
          loc.line, loc.col, "Cannot re-define attribute: " + methodId + "\n"));
      continue;
    }
    // An overriding method must keep the parameter types (except self) and
    // the return type of the method it overrides. Every class inherits
    // __init__(self: object) from object.
    bool sameSignature = true;
    if (const FunctionAST* overridden =
            lookupMethodInHierarchy(parentClass, methodId)) {
      const auto& args = method->getArgs();
      const auto& overriddenArgs = overridden->getArgs();
      sameSignature = args.size() == overriddenArgs.size() &&
                      method->getReturnType()->getTypeName() ==
                          overridden->getReturnType()->getTypeName();
      for (size_t i = 1; sameSignature && i < args.size(); ++i) {
        sameSignature = args[i]->getType()->getTypeName() ==
                        overriddenArgs[i]->getType()->getTypeName();
      }
    } else if (methodId == "__init__") {
      sameSignature = method->getArgs().size() == 1 &&
                      method->getReturnType()->getTypeName() == "<None>";
    }
    if (!sameSignature) {
      errors.push_back(SemanticError(
          loc.line, loc.col,
          "Method overridden with different type signature: " + methodId +
              "\n"));
    }
  }

  for (auto& method : clazz.getMethodDefs()) {
    functionStack.push(method.get());
    method->accept(*this);
    functionStack.pop();
  }
}

void SemanticCheckVisitor::visitFunction(const FunctionAST& func) {
  // A function inherits the bindings of its enclosing scopes and may shadow
  // them; they are restored once the function has been checked
  auto enclosingVarToType = localVarToType;
  auto enclosingScopeVars = currentScopeVars;
  currentScopeVars.clear();

  std::unordered_set<std::string> scopeNames;
  auto declare = [&](const std::string& name, const Location& loc) {
    if (isDefinedType(name)) {
      errors.push_back(SemanticError(
          loc.line, loc.col, "Cannot shadow class name: " + name + "\n"));
    } else if (!scopeNames.insert(name).second) {
      errors.push_back(SemanticError(
          loc.line, loc.col,
          "Duplicate declaration of identifier in same scope: " + name + "\n"));
    }
  };

  for (size_t argIndex = 0; argIndex < func.getArgs().size(); ++argIndex) {
    auto& arg = func.getArgs()[argIndex];
    arg->accept(*this);
    localVarToType[arg->getId().str()] = arg->getTypeInfo();
    currentScopeVars.insert(arg->getId().str());
    // nested functions take a hidden environment parameter first
    if (!(func.isNestedFunc() && argIndex == 0)) {
      declare(arg->getId().str(), arg->loc());
      // a method's self parameter is checked against its class below
      if (!(currentClass != nullptr && argIndex == 0)) {
        checkNotObjectAnnotation(arg->getType());
      }
    }
  }

  // [var-init]
  for (auto& localVar : func.getVarDefs()) {
    localVar->accept(*this);
    const std::string varId = localVar->getTypedVar()->getId().str();
    localVarToType[varId] = localVar->getTypedVar()->getType()->getTypeName();
    currentScopeVars.insert(varId);
    declare(varId, localVar->getTypedVar()->loc());
  }

  for (auto& globalId : func.getGlobalDecls()) {
    auto globalVar = globalVarToType.find(globalId);
    if (globalVar == globalVarToType.end()) {
      errors.push_back(
          SemanticError(func.loc().line, func.loc().col,
                        "Not a global variable: " + globalId + "\n"));
    } else {
      localVarToType[globalId] = globalVar->second;
      currentScopeVars.insert(globalId);
    }
    declare(globalId, func.loc());
  }

  for (auto& nonlocalId : func.getNonlocalDecls()) {
    auto nonlocalVar = enclosingVarToType.find(nonlocalId);
    if (!func.isNestedFunc() || nonlocalVar == enclosingVarToType.end()) {
      errors.push_back(
          SemanticError(func.loc().line, func.loc().col,
                        "Not a nonlocal variable: " + nonlocalId + "\n"));
    } else {
      localVarToType[nonlocalId] = nonlocalVar->second;
      currentScopeVars.insert(nonlocalId);
    }
    declare(nonlocalId, func.loc());
  }

  for (auto& nestedFunc : func.getFuncDefs()) {
    declare(nestedFunc->getId().str(), nestedFunc->loc());
  }

  const TypeAST* returnType = func.getReturnType();
  if (returnType->getTypeName() != "<None>" &&
      !isValidTypeAnnotation(returnType)) {
    errors.push_back(
        SemanticError(returnType->loc().line, returnType->loc().col,
                      "Undefined type: " + returnType->getTypeName() + "\n"));
  }
  checkNotObjectAnnotation(returnType);

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

  // Functions returning int, str or bool must return a value on every path
  if (isPrimitiveType(returnType->getTypeName()) &&
      !alwaysReturns(func.getBody())) {
    errors.push_back(SemanticError(
        func.loc().line, func.loc().col,
        "All paths in this function/method must have a return statement: " +
            func.getId().str() + "\n"));
  }

  localVarToType = std::move(enclosingVarToType);
  currentScopeVars = std::move(enclosingScopeVars);
}

bool SemanticCheckVisitor::alwaysReturns(
    const std::vector<std::unique_ptr<StmtAST>>& stmts) {
  for (const auto& stmt : stmts) {
    if (auto simpleStmt = llvm::dyn_cast<SimpleStmtAST>(stmt.get())) {
      auto returnStmt = llvm::dyn_cast<SimpleStmtReturnAST>(simpleStmt);
      if (returnStmt && returnStmt->getExpr()) {
        return true;
      }
    } else if (auto stmtIf = llvm::dyn_cast<StmtIfAST>(stmt.get())) {
      if (alwaysReturns(stmtIf->getBody()) &&
          alwaysReturns(stmtIf->getElseBody())) {
        return true;
      }
    }
  }
  return false;
}

// Values of primitive types are not boxed, so no variable, attribute,
// parameter, return value or list element may be declared as 'object'. It can
// only be used as a superclass.
void SemanticCheckVisitor::checkNotObjectAnnotation(const TypeAST* type) {
  if (auto listType = llvm::dyn_cast<ListTypeAST>(type)) {
    type = listType->getType();
  }
  if (type->getTypeName() == "object") {
    errors.push_back(
        SemanticError(type->loc().line, type->loc().col,
                      "Type 'object' can only be used as a superclass\n"));
  }
}

bool SemanticCheckVisitor::isValidTypeAnnotation(const TypeAST* type) {
  if (auto listType = llvm::dyn_cast<ListTypeAST>(type)) {
    return isDefinedType(listType->getType()->getTypeName());
  }
  return isDefinedType(type->getTypeName());
}

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
  // [list-display]: the element type is the join of the element types.
  // Nested list displays are joined on their innermost elements, so
  // [[A()], [B()]] is typed [[T]] with T the join of A and B.
  size_t dimension = 1;
  bool hasListElement = false;
  bool hasOtherElement = false;
  bool depthErrorReported = false;
  std::string elementType = "<Empty>";
  for (auto& element : listLiteralExpr.getElements()) {
    element->accept(*this);
    if (auto* inner = llvm::dyn_cast<ListLiteralExprAST>(element.get())) {
      hasListElement = true;
      dimension = std::max(dimension, inner->getListDimension() + 1);
      const std::string& innerType = inner->getTypeInfo();
      elementType =
          typeUnion(elementType, innerType.substr(0, innerType.find('[')));
    } else {
      hasOtherElement = true;
      elementType = typeUnion(elementType, element->getTypeInfo());
    }
    if (hasListElement && hasOtherElement && !depthErrorReported) {
      errors.push_back(SemanticError(
          element->loc().line, element->loc().col,
          "List elements must have the same depth in a list literal\n"));
      depthErrorReported = true;
    }
  }
  const_cast<ListLiteralExprAST&>(listLiteralExpr).setListDimension(dimension);
  // [nil]: the empty list has the special type <Empty>, written <Empty>[]
  std::string listType = elementType;
  for (size_t i = 0; i < dimension; ++i) {
    listType = listOf(listType);
  }
  listLiteralExpr.setTypeInfo(listType);
}

ClassAST* SemanticCheckVisitor::findClass(const std::string& classId) const {
  auto clazz = definedClasses.find(classId);
  return clazz == definedClasses.end() ? nullptr : clazz->second;
}

const FunctionAST*
SemanticCheckVisitor::resolveFunction(const std::string& name) const {
  // Look the name up from the innermost enclosing function outwards, then in
  // the global scope. Methods are not in scope without a receiver.
  std::vector<const FunctionAST*> enclosingFunctions;
  for (const FunctionAST* func = currentFunction(); func;
       func = func->getParentFunc()) {
    enclosingFunctions.push_back(func);
  }
  std::string prefix = currentClass ? currentClass->getId().str() + "-" : "";
  std::vector<std::string> scopePrefixes;
  for (auto func = enclosingFunctions.rbegin();
       func != enclosingFunctions.rend(); ++func) {
    prefix += (*func)->getId().str() + "-";
    scopePrefixes.push_back(prefix);
  }
  for (auto scope = scopePrefixes.rbegin(); scope != scopePrefixes.rend();
       ++scope) {
    auto func = definedFunctions.find(*scope + name);
    if (func != definedFunctions.end()) {
      return func->second;
    }
  }
  auto func = definedFunctions.find(name);
  return func == definedFunctions.end() ? nullptr : func->second;
}

void SemanticCheckVisitor::checkCallArgs(const CallExprAST& callExpr,
                                         const FunctionAST& func,
                                         size_t hiddenParams) {
  auto callee = llvm::cast<IdExprAST>(callExpr.getCallee());
  size_t paramCount = func.getArgs().size() - hiddenParams;
  if (callExpr.getArgs().size() != paramCount) {
    errors.push_back(
        SemanticError(callee->loc().line, callee->loc().col,
                      "Function '" + callee->getId().str() + "' expects " +
                          std::to_string(paramCount) + " arguments, but got " +
                          std::to_string(callExpr.getArgs().size()) + "\n"));
    return;
  }
  for (size_t argIndex = 0; argIndex < paramCount; ++argIndex) {
    auto argType = callExpr.getArgs()[argIndex]->getTypeInfo();
    auto expectedType =
        func.getArgs()[argIndex + hiddenParams]->getType()->getTypeName();
    if (!isAssignable(argType, expectedType)) {
      errors.push_back(SemanticError(
          callExpr.loc().line, callExpr.loc().col,
          "Argument " + std::to_string(argIndex + 1) + " of function '" +
              callee->getId().str() + "' must be of type '" + expectedType +
              "', but got '" + argType + "'\n"));
    }
  }
}

void SemanticCheckVisitor::visitCallExpr(const CallExprAST& callExpr) {
  for (const auto& arg : callExpr.getArgs()) {
    arg->accept(*this);
  }
  auto callee = llvm::dyn_cast<IdExprAST>(callExpr.getCallee());
  if (!callee) {
    return;
  }
  const std::string calleeId = callee->getId().str();

  // [dispatch]
  if (const ExprAST* selfExpr = callExpr.getSelfExpr()) {
    const FunctionAST* method =
        lookupMethodInHierarchy(findClass(selfExpr->getTypeInfo()), calleeId);
    if (!method) {
      errors.push_back(SemanticError(callee->loc().line, callee->loc().col,
                                     "Unknown method: " + calleeId + "\n"));
      return;
    }
    checkCallArgs(callExpr, *method, 1);
    callExpr.setTypeInfo(method->getReturnType()->getTypeName());
    return;
  }

  // [new]: arguments are passed on to __init__
  if (isDefinedType(calleeId)) {
    const FunctionAST* init =
        lookupMethodInHierarchy(findClass(calleeId), "__init__");
    if (init) {
      checkCallArgs(callExpr, *init, 1);
    } else if (!callExpr.getArgs().empty()) {
      errors.push_back(SemanticError(
          callee->loc().line, callee->loc().col,
          "Function '" + calleeId + "' expects 0 arguments, but got " +
              std::to_string(callExpr.getArgs().size()) + "\n"));
    }
    callExpr.setTypeInfo(calleeId);
    return;
  }

  // [invoke]
  if (const FunctionAST* funcPtr = resolveFunction(calleeId)) {
    checkCallArgs(callExpr, *funcPtr, funcPtr->isNestedFunc() ? 1 : 0);
    callExpr.setTypeInfo(funcPtr->getReturnType()->getTypeName());
    return;
  }

  // Predefined functions
  size_t expectedArgCount = calleeId == "input" ? 0 : 1;
  if (calleeId == "print" || calleeId == "len" || calleeId == "input") {
    if (callExpr.getArgs().size() != expectedArgCount) {
      errors.push_back(SemanticError(
          callee->loc().line, callee->loc().col,
          "Function '" + calleeId + "' expects " +
              std::to_string(expectedArgCount) + " arguments, but got " +
              std::to_string(callExpr.getArgs().size()) + "\n"));
    }
  }
  if (calleeId == "print") {
    for (const auto& arg : callExpr.getArgs()) {
      // an UNTYPED argument has already been reported
      if (arg->getTypeInfo() != "UNTYPED" &&
          !isPrimitiveType(arg->getTypeInfo())) {
        errors.push_back(SemanticError(arg->loc().line, arg->loc().col,
                                       "Argument of 'print' must be of "
                                       "type 'str', 'int', or 'bool'\n"));
      }
    }
    callExpr.setTypeInfo("<None>");
  } else if (calleeId == "len") {
    for (const auto& arg : callExpr.getArgs()) {
      if (arg->getTypeInfo() != "UNTYPED" && arg->getTypeInfo() != "str" &&
          !isListType(arg->getTypeInfo())) {
        errors.push_back(SemanticError(
            arg->loc().line, arg->loc().col,
            "Argument of 'len' must be of type 'str' or a list, but got '" +
                arg->getTypeInfo() + "'\n"));
      }
    }
    callExpr.setTypeInfo("int");
  } else if (calleeId == "input") {
    callExpr.setTypeInfo("str");
  } else {
    errors.push_back(SemanticError(callee->loc().line, callee->loc().col,
                                   "Unknown function: " + calleeId + "\n"));
  }
}

void SemanticCheckVisitor::visitIdExpr(const IdExprAST& idExpr) {
  // [var-read]
  const std::string id = idExpr.getId().str();
  if (localVarToType.find(id) != localVarToType.end()) {
    idExpr.setTypeInfo(localVarToType.at(id));
  } else if (globalVarToType.find(id) != globalVarToType.end()) {
    idExpr.setTypeInfo(globalVarToType.at(id));
  } else if (isDefinedType(id) || resolveFunction(id) || id == "print" ||
             id == "len" || id == "input") {
    // functions and classes are not first-class values
    errors.push_back(SemanticError(idExpr.loc().line, idExpr.loc().col,
                                   "Not a variable: " + id + "\n"));
  } else {
    errors.push_back(SemanticError(idExpr.loc().line, idExpr.loc().col,
                                   "Unknown identifier: " + id + "\n"));
  }
}

void SemanticCheckVisitor::visitBinaryExpr(const BinaryExprAST& binaryExpr) {
  binaryExpr.getLhs()->accept(*this);
  // set dispatch and type info; an attribute name is not a variable read
  if (binaryExpr.getOp() == TokenKind::kAttrAccessOp) {
    if (auto rhsCall = const_cast<CallExprAST*>(
            llvm::dyn_cast<CallExprAST>(binaryExpr.getRhs()))) {
      rhsCall->setSelfExpr(binaryExpr.getLhs());
      rhsCall->accept(*this);
    }
  } else {
    binaryExpr.getRhs()->accept(*this);
  }
  switch (binaryExpr.getOp()) {
  case TokenKind::kAttrAccessOp: {
    auto lhsType = binaryExpr.getLhs()->getTypeInfo();
    auto rhsId = llvm::dyn_cast<IdExprAST>(binaryExpr.getRhs());
    if (rhsId) {
      const VarDefAST* attr =
          lookupAttributeInHierarchy(findClass(lhsType), rhsId->getId().str());
      if (attr) {
        binaryExpr.setTypeInfo(attr->getTypedVar()->getType()->getTypeName());
      } else {
        errors.push_back(
            SemanticError(rhsId->loc().line, rhsId->loc().col,
                          "Unknown attribute: " + rhsId->getId().str() + "\n"));
      }
      return;
    }
    // method calls are resolved and checked in visitCallExpr
    if (auto rhsCall = llvm::dyn_cast<CallExprAST>(binaryExpr.getRhs())) {
      binaryExpr.setTypeInfo(rhsCall->getTypeInfo());
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
    if (!isPrimitiveType(lhsType) && !isPrimitiveType(rhsType)) {
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
  if (rhsType == "UNTYPED")
    return lhsType;
  if (isAssignable(lhsType, rhsType))
    return rhsType;
  if (isAssignable(rhsType, lhsType))
    return lhsType;

  // Otherwise the join is the least common ancestor in the class hierarchy
  std::unordered_set<std::string> lhsAncestors;
  for (const ClassAST* clazz = findClass(lhsType); clazz;
       clazz = clazz->getParentClass()) {
    if (clazz->getId() == "object")
      break;
    lhsAncestors.insert(clazz->getId().str());
  }
  for (const ClassAST* clazz = findClass(rhsType); clazz;
       clazz = clazz->getParentClass()) {
    if (clazz->getId() == "object")
      break;
    if (lhsAncestors.count(clazz->getId().str()))
      return clazz->getId().str();
  }
  return "object";
}

void SemanticCheckVisitor::visitVarDef(const VarDefAST& varDef) {
  varDef.getTypedVar()->accept(*this);
  checkNotObjectAnnotation(varDef.getTypedVar()->getType());
  auto type = varDef.getTypedVar()->getTypeInfo();
  if (currentClass == nullptr && currentFunction() == nullptr) {
    globalVarToType[varDef.getTypedVar()->getId().str()] = type;
  }
  // an undefined type has already been reported
  if (type == "UNTYPED") {
    return;
  }
  // [var-init]
  varDef.getLiteral()->accept(*this);
  if (isAssignable(varDef.getLiteral()->getTypeInfo(), type)) {
    return;
  }
  if (type == "str") {
    errors.push_back(SemanticError(
        varDef.getLiteral()->loc().line, varDef.getLiteral()->loc().col,
        "Variable '" + varDef.getTypedVar()->getId().str() +
            "' of type 'str' must be initialized with a string literal\n"));
  } else if (type == "int") {
    errors.push_back(SemanticError(
        varDef.getLiteral()->loc().line, varDef.getLiteral()->loc().col,
        "Variable '" + varDef.getTypedVar()->getId().str() +
            "' of type 'int' must be initialized with a number literal\n"));
  } else if (type == "bool") {
    errors.push_back(SemanticError(
        varDef.getLiteral()->loc().line, varDef.getLiteral()->loc().col,
        "Variable '" + varDef.getTypedVar()->getId().str() +
            "' of type 'bool' must be initialized with 'True' or 'False' "
            "literal values\n"));
  } else {
    errors.push_back(SemanticError(
        varDef.getLiteral()->loc().line, varDef.getLiteral()->loc().col,
        "Variable '" + varDef.getTypedVar()->getId().str() + "' of type '" +
            type + "' must be initialized with 'None' literal\n"));
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
        // [list-assign-stmt]: strings are immutable
        if (binaryExpr->getLhs()->getTypeInfo() == "str") {
          errors.push_back(
              SemanticError(target->loc().line, target->loc().col,
                            "Cannot assign to an index of a 'str' value\n"));
        }
      }
    } else if (auto idExpr = llvm::dyn_cast<IdExprAST>(target.get())) {
      // Variables inherited from an enclosing scope are read-only unless
      // declared global or nonlocal
      const std::string id = idExpr->getId().str();
      if (currentFunction() && !currentScopeVars.count(id) &&
          target->getTypeInfo() != "UNTYPED") {
        errors.push_back(SemanticError(
            target->loc().line, target->loc().col,
            "Cannot assign to variable that is not explicitly declared in "
            "this scope: " +
                id + "\n"));
      }
    }
  }
  simpleStmtAssign.getRhs()->accept(*this);
  // Check if the right-hand side expression is compatible with the
  // left-hand side targets
  auto rhsType = simpleStmtAssign.getRhs()->getTypeInfo();
  // [multi-assign-stmt]
  if (simpleStmtAssign.getTargets().size() > 1 && rhsType == "<None>[]") {
    const auto& firstTarget = simpleStmtAssign.getTargets().front();
    errors.push_back(SemanticError(
        firstTarget->loc().line, firstTarget->loc().col,
        "Right-hand side of multiple assignment may not be [<None>]\n"));
  }
  for (const auto& target : simpleStmtAssign.getTargets()) {
    auto targetType = target->getTypeInfo();
    if (!isAssignable(rhsType, targetType)) {
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
  // [for-str] / [for-list]: a loop variable declared in this scope must be
  // able to hold the elements; an undeclared one is declared implicitly
  const std::string id = stmtFor.getTypedVar()->getId().str();
  const std::string& elementType = stmtFor.getTypedVar()->getTypeInfo();
  auto& scopeVarToType = currentFunction() ? localVarToType : globalVarToType;
  bool declaredInScope = currentFunction() ? currentScopeVars.count(id) > 0
                                           : globalVarToType.count(id) > 0;
  if (declaredInScope && elementType != "UNTYPED" &&
      !isAssignable(elementType, scopeVarToType.at(id))) {
    errors.push_back(SemanticError(
        stmtFor.getTypedVar()->loc().line, stmtFor.getTypedVar()->loc().col,
        "Type mismatch: cannot assign '" + elementType + "' to '" +
            scopeVarToType.at(id) + "'\n"));
  }
  scopeVarToType[id] = elementType;
  if (currentFunction()) {
    currentScopeVars.insert(id);
  }
  for (const auto& forBodyStmt : stmtFor.getBody()) {
    forBodyStmt->accept(*this);
  }
}

void SemanticCheckVisitor::visitSimpleStmtReturn(
    const SimpleStmtReturnAST& simpleStmtReturn) {
  if (simpleStmtReturn.getExpr())
    simpleStmtReturn.getExpr()->accept(*this);
  if (!currentFunction()) {
    errors.push_back(
        SemanticError(simpleStmtReturn.loc().line, simpleStmtReturn.loc().col,
                      "Return statement cannot appear at the top level\n"));
    return;
  }
  // [return-e] / [return]
  auto retType = simpleStmtReturn.getExpr()
                     ? simpleStmtReturn.getExpr()->getTypeInfo()
                     : "<None>";
  auto funcReturnType = currentFunction()->getReturnType()->getTypeName();
  if (!isAssignable(retType, funcReturnType)) {
    errors.push_back(
        SemanticError(simpleStmtReturn.loc().line, simpleStmtReturn.loc().col,
                      "Return type mismatch: expected '" + funcReturnType +
                          "', found '" + retType + "'\n"));
  }
}

bool SemanticCheckVisitor::isSubClassOf(const std::string& subType,
                                        const std::string& superType) {
  if (subType == superType)
    return true;
  // every type, including list types, <None> and <Empty>, conforms to object
  if (superType == "object")
    return subType != "UNTYPED";
  for (const ClassAST* clazz = findClass(subType); clazz;
       clazz = clazz->getParentClass()) {
    if (clazz->getId() == "object")
      break;
    if (clazz->getId() == superType)
      return true;
  }
  return false;
}

bool SemanticCheckVisitor::isAssignable(const std::string& subType,
                                        const std::string& superType) {
  if (isSubClassOf(subType, superType))
    return true;
  // None may be assigned to anything but int, bool and str
  if (subType == "<None>")
    return !isPrimitiveType(superType);
  // [] may be assigned to any list type, and a nested display of empty
  // lists such as [[]] to any list type of the same depth
  if (subType == "<Empty>[]")
    return isListType(superType);
  if (subType.rfind("<Empty>[", 0) == 0 && isListType(superType))
    return subType.size() - subType.find('[') ==
           superType.size() - superType.find('[');
  // [None, ...] may be assigned to [T] when None may be assigned to T
  if (subType == "<None>[]" && isListType(superType))
    return isAssignable("<None>", getInnerType(superType));
  return false;
}

const FunctionAST* SemanticCheckVisitor::currentFunction() const {
  return functionStack.empty() ? nullptr : functionStack.top();
}

} // namespace chocopy
