#include <filesystem>
#include <iostream>

#include "CodeGen.h"

namespace chocopy {

LLVMCodeGenVisitor::LLVMCodeGenVisitor(ProgramAST* program,
                                       llvm::StringRef programPath)
    : context(std::make_unique<llvm::LLVMContext>()),
      builder(std::make_unique<llvm::IRBuilder<>>(*context)),
      programAST(program), programPath(programPath) {}

LLVMCodeGenVisitor::~LLVMCodeGenVisitor() {}

void LLVMCodeGenVisitor::printLLVMBitCode(llvm::StringRef outputPath) const {
  std::error_code error;
  llvm::raw_fd_ostream dest(outputPath.str() + ".ll", error);
  if (error) {
    llvm::errs() << "Failed to print LLVM bitcode" << error.message() << "\n";
    return;
  }
  module->print(dest, nullptr);
}

const VirtualTable& LLVMCodeGenVisitor::getVTable(const ClassAST* classPtr) {
  return classToVTable.at(classPtr);
}

const VirtualTable&
LLVMCodeGenVisitor::getVTable(const std::string& className) {
  return getVTable(getClassByName(className));
}

void LLVMCodeGenVisitor::createClassTypesAndVtableTypes(
    const std::vector<std::unique_ptr<ClassAST>>& classDefs) {
  for (auto& clazz : classDefs) {
    classToVTable.insert(
        std::make_pair(clazz.get(), VirtualTable(*context, clazz.get())));
    classToStructType.insert(std::make_pair(
        clazz.get(), llvm::StructType::create(*context, clazz->getId())));
  }
}

void LLVMCodeGenVisitor::codeGen() {
  std::filesystem::path path(programPath.data());
  std::string fileName = path.filename().string();

  module = std::make_unique<llvm::Module>(fileName, *context);
  module->setTargetTriple("x86_64-pc-linux-gnu");

  // object class inclusion
  classToVTable.insert(
      std::make_pair(programAST->getObjectClass(),
                     VirtualTable(*context, programAST->getObjectClass())));
  classToStructType.insert(
      std::make_pair(programAST->getObjectClass(),
                     llvm::StructType::create(
                         *context, programAST->getObjectClass()->getId())));

  llvm::Type* vtablePtr = classToVTable.at(programAST->getObjectClass())
                              .GetVTStructType()
                              ->getPointerTo();
  std::vector<llvm::Type*> attributeTypes = {vtablePtr};
  classToStructType.at(programAST->getObjectClass())->setBody(attributeTypes);

  createClassTypesAndVtableTypes(programAST->getClassDefs());

  /* Include external function declarations */
  createBuiltinFuncDecl("puts", "int", {"str"});
  createBuiltinFuncDecl("malloc", "str", {"int"});
  createBuiltinFuncDecl("printf", "int", {"str"}, true);
  createBuiltinFuncDecl("strcmp", "int", {"str", "str"});

  programAST->accept(*this);
}

void LLVMCodeGenVisitor::visitProgram(const ProgramAST& program) {
  for (auto& globalVarDef : program.getVarDefs()) {
    globalVarDef->accept(*this);
  }

  for (auto& clazz : program.getClassDefs()) {
    currentClass = clazz.get();

    for (const auto& attributeDef : clazz->getVarDefs()) {
      attributeDef->accept(*this);
    }

    // add attributes and methods
    addAttributes(clazz.get());
    addMethods(clazz.get());

    currentClass = nullptr;
  }

  for (auto& clazz : program.getClassDefs()) {
    currentClass = const_cast<ClassAST*>(clazz.get());
    clazz->accept(*this);
    currentClass = nullptr;
  }

  for (auto& globFunc : program.getFuncDefs()) {
    currentFunction = globFunc.get();

    llvm::Type* retType =
        llvmTypeOrClassPtrType(globFunc->getReturnType()->getTypeName());

    std::vector<llvm::Type*> argTypes;
    for (const auto& arg : globFunc->getArgs()) {
      argTypes.push_back(llvmTypeOrClassPtrType(arg->getType()->getTypeName()));
    }

    std::string funcName = globFunc->getId().str();
    llvm::FunctionType* funcType =
        llvm::FunctionType::get(retType, argTypes, false);
    llvm::Function* func = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, funcName, *module);

    functionNameToFunc[funcName] = func;
    functions[globFunc.get()] = func;

    globFunc->accept(*this);

    currentFunction = nullptr;
  }

  codeGenMainFunc(program.getStmts());
}

void LLVMCodeGenVisitor::addAttributes(const ClassAST* classPtr) {
  std::vector<llvm::Type*> attributeTypes;

  const ClassAST* parentClassPtr = classPtr->getParentClass();
  std::unordered_map<std::string,
                     std::pair<std::vector<llvm::Value*>, const VarDefAST*>>
      attrIndexMap;

  attributeTypes.push_back(classToStructType.at(parentClassPtr));

  auto makeIndex = [this](uint32_t idx) {
    return llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context), idx);
  };

  if (parentClassPtr != programAST->getObjectClass()) {
    const auto& parentMap = classFieldGEPMap.at(parentClassPtr);
    for (const auto& kv : parentMap) {
      std::vector<llvm::Value*> gep = kv.second.first;
      gep.insert(gep.begin(), makeIndex(0));
      attrIndexMap[kv.first] = std::make_pair(std::move(gep), kv.second.second);
    }
  }

  int fieldIndex = 1;
  for (const auto& attribute : classPtr->getVarDefs()) {
    attributeTypes.push_back(llvmTypeOrClassPtrType(
        attribute->getTypedVar()->getType()->getTypeName()));

    std::vector<llvm::Value*> gep = {makeIndex(0), makeIndex(fieldIndex)};
    attrIndexMap[attribute->getTypedVar()->getId().str()] =
        std::make_pair(std::move(gep), attribute.get());
    fieldIndex++;
  }

  classToStructType.at(classPtr)->setBody(attributeTypes);
  classFieldGEPMap[classPtr] = std::move(attrIndexMap);
}

void LLVMCodeGenVisitor::addMethods(const ClassAST* classPtr) {
  std::vector<llvm::Constant*> vtableFuncs;

  if (classPtr->getSuperClassId().str() != "object") {
    const std::vector<llvm::Constant*>& superClassVTableFuncs =
        getVTable(classPtr->getSuperClassId().str()).getFuncs();

    vtableFuncs.insert(vtableFuncs.end(), superClassVTableFuncs.begin(),
                       superClassVTableFuncs.end());
  }

  // extend the ability to lookup the inherited function using current class
  // name and method combined name
  for (const auto& inheritedFunc : vtableFuncs) {
    if (auto funcPtr = llvm::dyn_cast<llvm::Function>(inheritedFunc)) {
      std::string existingName = funcPtr->getName().str();
      size_t dashPos = existingName.find('-');
      if (dashPos != std::string::npos) {
        std::string existingMethodName = existingName.substr(dashPos + 1);
        std::string methodLookupNameForClass =
            classPtr->getId().str() + "-" + existingMethodName;
        functionNameToFunc[methodLookupNameForClass] = funcPtr;
      }
    }
  }

  size_t inheritedVTableSize = vtableFuncs.size();

  for (const auto& methodDef : classPtr->getMethodDefs()) {
    llvm::Type* retType =
        llvmTypeOrClassPtrType(methodDef->getReturnType()->getTypeName());

    std::vector<llvm::Type*> argTypes;
    for (const auto& arg : methodDef->getArgs()) {
      argTypes.push_back(llvmTypeOrClassPtrType(arg->getType()->getTypeName()));
    }

    std::string funcName =
        classPtr->getId().str() + "-" + methodDef->getId().str();
    llvm::FunctionType* funcType =
        llvm::FunctionType::get(retType, argTypes, false);
    llvm::Function* func = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, funcName, *module);

    functionNameToFunc[funcName] = func;

    functions[methodDef.get()] = func;
    bool isOverride = false;
    size_t vtableIndex = 0;

    for (; vtableIndex < inheritedVTableSize; vtableIndex++) {
      const auto& existingFunc = vtableFuncs[vtableIndex];
      std::string existingName = existingFunc->getName().str();
      size_t dashPos = existingName.find('-');
      if (dashPos != std::string::npos) {
        std::string existingMethodName = existingName.substr(dashPos + 1);
        if (existingMethodName == methodDef->getId().str()) {
          llvm::Function* existingFuncPtr =
              llvm::dyn_cast<llvm::Function>(existingFunc);
          if (existingFuncPtr &&
              existingFuncPtr->getFunctionType() == funcType) {
            isOverride = true;
            break;
          }
        }
      }
    }

    if (isOverride) {
      vtableFuncs[vtableIndex] = func;
    } else {
      vtableFuncs.push_back(func);
    }
  }
  classToVTable.at(classPtr).createVTable(module.get(), vtableFuncs);
}

void LLVMCodeGenVisitor::visitClass(const ClassAST& clazz) {
  for (const auto& func : clazz.getMethodDefs()) {
    func->accept(*this);
  }
}

void LLVMCodeGenVisitor::visitFunction(const FunctionAST& func) {
  currentFunction = const_cast<FunctionAST*>(&func);
  llvm::Function* llvmFunction = llvmFunc(&func);

  llvm::BasicBlock* entry =
      llvm::BasicBlock::Create(*context, "entrypoint", llvmFunction);
  builder->SetInsertPoint(entry);

  for (auto& param : llvmFunction->args()) {
    int paramIndex = param.getArgNo();
    llvm::Type* paramType =
        llvmFunction->getFunctionType()->getParamType(paramIndex);
    llvm::AllocaInst* alloca = builder->CreateAlloca(
        paramType, nullptr, func.getArgs().at(paramIndex)->getId());
    localVariables[func.getArgs().at(paramIndex)->getId()] = alloca;
    localVariableType[func.getArgs().at(paramIndex)->getId()] =
        llvm::StringRef(func.getArgs()[paramIndex]->getType()->getTypeName());
    builder->CreateStore(&param, alloca);
  }

  for (auto& localVar : func.getVarDefs()) {
    localVar->accept(*this);
  }

  for (const auto& stmt : func.getBody()) {
    stmt->accept(*this);
  }

  if (!builder->GetInsertBlock()->getTerminator()) {
    builder->CreateRet(llvmDefaultValue(func.getReturnType()->getTypeName()));
  }
  currentFunction = nullptr;
};

void LLVMCodeGenVisitor::codeGenMainFunc(
    const std::vector<std::unique_ptr<StmtAST>>& stmts) {
  llvm::FunctionType* funcType =
      llvm::FunctionType::get(llvm::Type::getInt32Ty(*context), false);
  llvm::Function* mainFunc = llvm::Function::Create(
      funcType, llvm::Function::ExternalLinkage, "main", module.get());
  llvm::BasicBlock* entryBlock =
      llvm::BasicBlock::Create(*context, "entry", mainFunc);
  builder->SetInsertPoint(entryBlock);

  for (const auto& stmt : stmts) {
    stmt->accept(*this);
  }

  builder->CreateRet(
      llvm::ConstantInt::getSigned(llvm::Type::getInt32Ty(*context), 0));
}

void LLVMCodeGenVisitor::visitLiteralNumber(
    const LiteralNumberAST& literalNumber) {
  llvm::Value* codegenValue = llvm::ConstantInt::getSigned(
      llvm::Type::getInt32Ty(*context), literalNumber.getNumber());
  literalNumber.setCodegenValue(codegenValue);
}

void LLVMCodeGenVisitor::visitLiteralTrue(const LiteralTrueAST& literalTrue) {
  llvm::Value* codegenValue = llvm::ConstantInt::getTrue(*context);
  literalTrue.setCodegenValue(codegenValue);
}

void LLVMCodeGenVisitor::visitLiteralFalse(
    const LiteralFalseAST& literalFalse) {
  llvm::Value* codegenValue = llvm::ConstantInt::getFalse(*context);
  literalFalse.setCodegenValue(codegenValue);
}

void LLVMCodeGenVisitor::visitLiteralString(
    const LiteralStringAST& literalString) {
  const std::string& str = literalString.getStr().str();

  llvm::GlobalVariable* globalString = nullptr;
  auto it = stringLiteralMap.find(str);
  if (it != stringLiteralMap.end()) {
    globalString = it->second;
  } else {
    llvm::Constant* stringConstant =
        llvm::ConstantDataArray::getString(module->getContext(), str, true);

    globalString = new llvm::GlobalVariable(
        *module, stringConstant->getType(), true,
        llvm::GlobalValue::PrivateLinkage, stringConstant, ".str");

    stringLiteralMap[str] = globalString;
  }

  llvm::Value* stringPtr = builder->CreateConstGEP1_32(
      globalString->getValueType(), globalString, 0, ".str_ptr");
  literalString.setCodegenValue(stringPtr);
}

void LLVMCodeGenVisitor::visitLiteralNone(const LiteralNoneAST& literalNone) {
  literalNone.setCodegenValue(llvm::ConstantPointerNull::get(
      llvmTypeOrClassPtrType("object")->getPointerTo()));
}

void LLVMCodeGenVisitor::visitBinaryExpr(const BinaryExprAST& binaryExpr) {
  switch (binaryExpr.getOp()) {
  case TokenKind::kAttrAccessOp: {
    if (auto rhs = llvm::dyn_cast<CallExprAST>(binaryExpr.getRhs())) {
      binaryExpr.getLhs()->accept(*this);
      std::string instanceType = binaryExpr.getLhs()->getTypeInfo();

      llvm::Value* instancePtr = builder->CreateLoad(
          getVTable(instanceType).GetVTStructType()->getPointerTo(),
          binaryExpr.getLhs()->getCodegenValue(), "current_instance_ptr");

      auto funcId = llvm::dyn_cast<IdExprAST>(rhs->getCallee())->getId().str();
      std::string functionName = instanceType + "-" + funcId;
      auto llvmFunc = functionNameToFunc[functionName];

      size_t vTableIndex = getVTable(instanceType).getVTableIndex(llvmFunc);

      for (const auto& arg : rhs->getArgs()) {
        arg->accept(*this);
      }

      // first arg is always the instance ptr
      std::vector<llvm::Value*> args = {instancePtr};
      for (const auto& arg : rhs->getArgs()) {
        llvm::Value* argVal = arg->getCodegenValue();
        if (argVal == nullptr) {
          llvm::errs() << "Unknown argument";
          return;
        }
        args.push_back(argVal);
      }

      llvm::Value* vtablePtrPtr = builder->CreateStructGEP(
          llvmClass(instanceType), instancePtr, 0, "vtable_ptr");
      llvm::Value* vtablePtr = builder->CreateLoad(
          getVTable(instanceType).GetVTStructType()->getPointerTo(),
          vtablePtrPtr, "vtable");

      llvm::Value* funcPtrAddr =
          builder->CreateStructGEP(getVTable(instanceType).GetVTStructType(),
                                   vtablePtr, vTableIndex, "func_ptr_addr");
      llvm::Value* funcPtr =
          builder->CreateLoad(llvmFunc->getType(), funcPtrAddr, "func_ptr");

      llvm::FunctionType* funcType = llvmFunc->getFunctionType();
      llvm::Value* val = builder->CreateCall(funcType, funcPtr, args);
      binaryExpr.setCodegenValue(val);
    } else if (auto rhs = llvm::dyn_cast<IdExprAST>(binaryExpr.getRhs())) {
      // attribute access
      binaryExpr.getLhs()->accept(*this);
      std::string instanceType = binaryExpr.getLhs()->getTypeInfo();
      auto classPtr = getClassByName(instanceType);
      llvm::Value* instancePtr = builder->CreateLoad(
          llvmClass(instanceType)->getPointerTo(),
          binaryExpr.getLhs()->getCodegenValue(), "current_instance_ptr");

      const auto& attributeGEP =
          classFieldGEPMap.at(classPtr)[rhs->getId().str()].first;
      llvm::Value* fieldGEP = builder->CreateGEP(
          llvmClass(instanceType), instancePtr, attributeGEP, "field_gep");

      std::string fieldTypeName =
          classFieldGEPMap.at(classPtr)[rhs->getId().str()]
              .second->getTypedVar()
              ->getType()
              ->getTypeName();
      llvm::Type* fieldType = llvmTypeOrClassPtrType(fieldTypeName);
      // only load primitive types
      if (!llvmClass(fieldTypeName)) {
        fieldGEP = builder->CreateLoad(fieldType, fieldGEP, "field_val");
        binaryExpr.setCodegenValue(fieldGEP);
      } else {
        binaryExpr.setCodegenValue(fieldGEP);
      }
    }
    return;
  }
  case TokenKind::kPlus: {
    binaryExpr.getLhs()->accept(*this);
    binaryExpr.getRhs()->accept(*this);
    llvm::Value* lhsVal = binaryExpr.getLhs()->getCodegenValue();
    llvm::Value* rhsVal = binaryExpr.getRhs()->getCodegenValue();
    if (lhsVal == nullptr || rhsVal == nullptr) {
      llvm::errs() << "Unknown operands in binary expression\n";
      return;
    }
    if (binaryExpr.getLhs()->getTypeInfo() == "str" &&
        binaryExpr.getRhs()->getTypeInfo() == "str") {
      // TODO: support string concatenation
    } else if (binaryExpr.getLhs()->getTypeInfo() == "int" &&
               binaryExpr.getRhs()->getTypeInfo() == "int") {
      llvm::Value* sumVal = builder->CreateAdd(lhsVal, rhsVal, "sum_ints");
      binaryExpr.setCodegenValue(sumVal);
    } else {
      llvm::errs() << "Unsupported types for '+' operator\n";
    }
    return;
  }
  case TokenKind::kMinus: {
    binaryExpr.getLhs()->accept(*this);
    binaryExpr.getRhs()->accept(*this);
    llvm::Value* lhsVal = binaryExpr.getLhs()->getCodegenValue();
    llvm::Value* rhsVal = binaryExpr.getRhs()->getCodegenValue();
    llvm::Value* diffVal = builder->CreateSub(lhsVal, rhsVal, "diff_ints");
    binaryExpr.setCodegenValue(diffVal);
    return;
  }
  case TokenKind::kMul: {
    binaryExpr.getLhs()->accept(*this);
    binaryExpr.getRhs()->accept(*this);
    llvm::Value* lhsVal = binaryExpr.getLhs()->getCodegenValue();
    llvm::Value* rhsVal = binaryExpr.getRhs()->getCodegenValue();
    llvm::Value* prodVal = builder->CreateMul(lhsVal, rhsVal, "prod_ints");
    binaryExpr.setCodegenValue(prodVal);
    return;
  }
  case TokenKind::kIntDiv: {
    binaryExpr.getLhs()->accept(*this);
    binaryExpr.getRhs()->accept(*this);
    llvm::Value* lhsVal = binaryExpr.getLhs()->getCodegenValue();
    llvm::Value* rhsVal = binaryExpr.getRhs()->getCodegenValue();
    llvm::Value* divVal = builder->CreateSDiv(lhsVal, rhsVal, "div_ints");
    binaryExpr.setCodegenValue(divVal);
    return;
  }
  case TokenKind::kMod: {
    binaryExpr.getLhs()->accept(*this);
    binaryExpr.getRhs()->accept(*this);
    llvm::Value* lhsVal = binaryExpr.getLhs()->getCodegenValue();
    llvm::Value* rhsVal = binaryExpr.getRhs()->getCodegenValue();
    llvm::Value* modVal = builder->CreateSRem(lhsVal, rhsVal, "mod_ints");
    binaryExpr.setCodegenValue(modVal);
    return;
  }
  case TokenKind::kLessThan: {
    binaryExpr.getLhs()->accept(*this);
    binaryExpr.getRhs()->accept(*this);
    llvm::Value* lhsVal = binaryExpr.getLhs()->getCodegenValue();
    llvm::Value* rhsVal = binaryExpr.getRhs()->getCodegenValue();
    if (lhsVal == nullptr || rhsVal == nullptr) {
      llvm::errs() << "Unknown operands in binary expression\n";
      return;
    }
    llvm::Value* cmpVal =
        builder->CreateICmpSLT(lhsVal, rhsVal, "cmp_less_than");
    binaryExpr.setCodegenValue(cmpVal);
    return;
  }
  case TokenKind::kGreaterThan: {
    binaryExpr.getLhs()->accept(*this);
    binaryExpr.getRhs()->accept(*this);
    llvm::Value* lhsVal = binaryExpr.getLhs()->getCodegenValue();
    llvm::Value* rhsVal = binaryExpr.getRhs()->getCodegenValue();
    if (lhsVal == nullptr || rhsVal == nullptr) {
      llvm::errs() << "Unknown operands in binary expression\n";
      return;
    }
    llvm::Value* cmpVal =
        builder->CreateICmpSGT(lhsVal, rhsVal, "cmp_greater_than");
    binaryExpr.setCodegenValue(cmpVal);
    return;
  }
  case TokenKind::kLessThanOrEqual: {
    binaryExpr.getLhs()->accept(*this);
    binaryExpr.getRhs()->accept(*this);
    llvm::Value* lhsVal = binaryExpr.getLhs()->getCodegenValue();
    llvm::Value* rhsVal = binaryExpr.getRhs()->getCodegenValue();
    if (lhsVal == nullptr || rhsVal == nullptr) {
      llvm::errs() << "Unknown operands in binary expression\n";
      return;
    }
    llvm::Value* cmpVal =
        builder->CreateICmpSLE(lhsVal, rhsVal, "cmp_less_than_or_equal");
    binaryExpr.setCodegenValue(cmpVal);
    return;
  }
  case TokenKind::kGreaterThanOrEqual: {
    binaryExpr.getLhs()->accept(*this);
    binaryExpr.getRhs()->accept(*this);
    llvm::Value* lhsVal = binaryExpr.getLhs()->getCodegenValue();
    llvm::Value* rhsVal = binaryExpr.getRhs()->getCodegenValue();
    if (lhsVal == nullptr || rhsVal == nullptr) {
      llvm::errs() << "Unknown operands in binary expression\n";
      return;
    }
    llvm::Value* cmpVal =
        builder->CreateICmpSGE(lhsVal, rhsVal, "cmp_greater_than_or_equal");
    binaryExpr.setCodegenValue(cmpVal);
    return;
  }
  case TokenKind::k_and: {
    binaryExpr.getLhs()->accept(*this);
    binaryExpr.getRhs()->accept(*this);
    llvm::Value* lhsVal = binaryExpr.getLhs()->getCodegenValue();
    llvm::Value* rhsVal = binaryExpr.getRhs()->getCodegenValue();
    if (lhsVal == nullptr || rhsVal == nullptr) {
      llvm::errs() << "Unknown operands in binary expression\n";
      return;
    }
    llvm::Value* andVal = builder->CreateAnd(lhsVal, rhsVal, "and_expr_val");
    binaryExpr.setCodegenValue(andVal);
    return;
  }
  case TokenKind::k_or: {
    binaryExpr.getLhs()->accept(*this);
    binaryExpr.getRhs()->accept(*this);
    llvm::Value* lhsVal = binaryExpr.getLhs()->getCodegenValue();
    llvm::Value* rhsVal = binaryExpr.getRhs()->getCodegenValue();
    if (lhsVal == nullptr || rhsVal == nullptr) {
      llvm::errs() << "Unknown operands in binary expression\n";
      return;
    }
    llvm::Value* orVal = builder->CreateOr(lhsVal, rhsVal, "or_expr_val");
    binaryExpr.setCodegenValue(orVal);
    return;
  }
  case TokenKind::kEqual: {
    binaryExpr.getLhs()->accept(*this);
    binaryExpr.getRhs()->accept(*this);
    llvm::Value* lhsVal = binaryExpr.getLhs()->getCodegenValue();
    llvm::Value* rhsVal = binaryExpr.getRhs()->getCodegenValue();
    if (lhsVal == nullptr || rhsVal == nullptr) {
      llvm::errs() << "Unknown operands in binary expression\n";
      return;
    }

    auto lhsType = binaryExpr.getLhs()->getTypeInfo();
    if (lhsType == "str") {
      llvm::Function* strCmpFunc = module->getFunction("strcmp");
      llvm::Value* cmpVal =
          builder->CreateCall(strCmpFunc, {lhsVal, rhsVal}, "strcmp_call");
      llvm::Value* isEqual = builder->CreateICmpEQ(
          cmpVal, llvm::ConstantInt::get(cmpVal->getType(), 0), "str_eq");
      binaryExpr.setCodegenValue(isEqual);
      return;
    }

    llvm::Value* eqVal = builder->CreateICmpEQ(lhsVal, rhsVal, "cmp_equal");
    binaryExpr.setCodegenValue(eqVal);
    return;
  }
  case TokenKind::kInEqual: {
    binaryExpr.getLhs()->accept(*this);
    binaryExpr.getRhs()->accept(*this);
    llvm::Value* lhsVal = binaryExpr.getLhs()->getCodegenValue();
    llvm::Value* rhsVal = binaryExpr.getRhs()->getCodegenValue();
    if (lhsVal == nullptr || rhsVal == nullptr) {
      llvm::errs() << "Unknown operands in binary expression\n";
      return;
    }

    auto lhsType = binaryExpr.getLhs()->getTypeInfo();
    if (lhsType == "str") {
      llvm::Function* strCmpFunc = module->getFunction("strcmp");
      llvm::Value* cmpVal =
          builder->CreateCall(strCmpFunc, {lhsVal, rhsVal}, "strcmp_call");
      llvm::Value* isEqual = builder->CreateICmpNE(
          cmpVal, llvm::ConstantInt::get(cmpVal->getType(), 0), "str_eq");
      binaryExpr.setCodegenValue(isEqual);
      return;
    }

    llvm::Value* neqVal =
        builder->CreateICmpNE(lhsVal, rhsVal, "cmp_not_equal");
    binaryExpr.setCodegenValue(neqVal);
    return;
  }
  case TokenKind::k_is: {
    binaryExpr.getLhs()->accept(*this);
    binaryExpr.getRhs()->accept(*this);
    llvm::Value* lhsVal = binaryExpr.getLhs()->getCodegenValue();
    llvm::Value* rhsVal = binaryExpr.getRhs()->getCodegenValue();
    if (lhsVal == nullptr || rhsVal == nullptr) {
      llvm::errs() << "Unknown operands in 'is' expression\n";
      return;
    }
    if (llvmClass(binaryExpr.getLhs()->getTypeInfo())) {
      lhsVal = builder->CreateLoad(lhsVal->getType()->getPointerTo(), lhsVal,
                                   "lhs_loaded");
    }
    if (llvmClass(binaryExpr.getRhs()->getTypeInfo())) {
      rhsVal = builder->CreateLoad(rhsVal->getType()->getPointerTo(), rhsVal,
                                   "rhs_loaded");
    }
    llvm::Value* isVal = builder->CreateICmpEQ(lhsVal, rhsVal, "is_expr_val");
    binaryExpr.setCodegenValue(isVal);
    return;
  }
  }
}

void LLVMCodeGenVisitor::visitUnaryExpr(const UnaryExprAST& unaryExpr) {
  unaryExpr.getExpr()->accept(*this);
  llvm::Value* exprVal = unaryExpr.getExpr()->getCodegenValue();
  if (exprVal == nullptr) {
    llvm::errs() << "Unknown operand in unary expression\n";
    return;
  }

  switch (unaryExpr.getOp()) {
  case TokenKind::k_not: {
    llvm::Value* notVal = builder->CreateNot(exprVal, "not_expr_val");
    unaryExpr.setCodegenValue(notVal);
    break;
  }
  case TokenKind::kMinus: {
    llvm::Value* negVal = builder->CreateNeg(exprVal, "neg_expr_val");
    unaryExpr.setCodegenValue(negVal);
    break;
  }
  default:
    llvm::errs() << "Unknown unary operator\n";
    return;
  }
}

void LLVMCodeGenVisitor::visitIfElseExpr(const IfElseExprAST& ifElseExpr) {
  ifElseExpr.getCondition()->accept(*this);
  llvm::Value* condVal = ifElseExpr.getCondition()->getCodegenValue();
  if (!condVal) {
    llvm::errs() << "Unknown condition in if-else expression\n";
    return;
  }

  llvm::Function* currentFunc = builder->GetInsertBlock()->getParent();
  llvm::BasicBlock* thenBlock =
      llvm::BasicBlock::Create(*context, "then", currentFunc);
  llvm::BasicBlock* elseBlock =
      llvm::BasicBlock::Create(*context, "else", currentFunc);
  llvm::BasicBlock* mergeBlock =
      llvm::BasicBlock::Create(*context, "if_merge", currentFunc);

  builder->CreateCondBr(condVal, thenBlock, elseBlock);

  builder->SetInsertPoint(thenBlock);
  ifElseExpr.getIfBody()->accept(*this);
  llvm::Value* thenVal = ifElseExpr.getIfBody()->getCodegenValue();
  builder->CreateBr(mergeBlock);
  thenBlock = builder->GetInsertBlock();

  builder->SetInsertPoint(elseBlock);
  ifElseExpr.getElseBody()->accept(*this);
  llvm::Value* elseVal = ifElseExpr.getElseBody()->getCodegenValue();
  builder->CreateBr(mergeBlock);
  elseBlock = builder->GetInsertBlock();

  builder->SetInsertPoint(mergeBlock);
  llvm::PHINode* phi = builder->CreatePHI(thenVal->getType(), 2, "if_expr_val");
  phi->addIncoming(thenVal, thenBlock);
  phi->addIncoming(elseVal, elseBlock);

  ifElseExpr.setCodegenValue(phi);
}

void LLVMCodeGenVisitor::visitIdExpr(const IdExprAST& idExpr) {
  if (currentClass == nullptr && currentFunction == nullptr) {
    llvm::GlobalVariable* globalVar = globalVariables[idExpr.getId()];
    if (globalVar == nullptr) {
      llvm::errs() << "Unknown global variable: " << idExpr.getId() << "\n";
      return;
    }
    llvm::Type* varType = globalVar->getValueType();
    if (globalVariableTypes[idExpr.getId()] == "str" ||
        globalVariableTypes[idExpr.getId()] == "int" ||
        globalVariableTypes[idExpr.getId()] == "bool") {
      llvm::Value* globalVarVal =
          builder->CreateLoad(varType, globalVar, "global_var_val");
      idExpr.setCodegenValue(globalVarVal);
    } else {
      idExpr.setCodegenValue(globalVar);
    }
  } else if (currentClass != nullptr && currentFunction != nullptr) {
    if (idExpr.getId() == "self") {
      idExpr.setCodegenValue(localVariables[idExpr.getId()]);
    } else {
      llvm::Value* localVar = localVariables[idExpr.getId()];
      if (localVar == nullptr) {
        llvm::errs() << "Unknown local variable: " << idExpr.getId() << "\n";
        return;
      }
      // only load primitive types
      if (!llvmClass(localVariableType[idExpr.getId()])) {
        llvm::Value* localVarVal = builder->CreateLoad(
            static_cast<llvm::AllocaInst*>(localVar)->getAllocatedType(),
            localVar, "local_var_val");
        idExpr.setCodegenValue(localVarVal);
      } else {
        idExpr.setCodegenValue(localVar);
      }
    }
  } else if (currentClass == nullptr && currentFunction != nullptr) {
    llvm::Value* localVar = localVariables[idExpr.getId()];
    if (localVar != nullptr) {
      // only load primitive types
      if (!llvmClass(localVariableType[idExpr.getId()])) {
        llvm::Value* localVarVal = builder->CreateLoad(
            static_cast<llvm::AllocaInst*>(localVar)->getAllocatedType(),
            localVar, "local_var_val");
        idExpr.setCodegenValue(localVarVal);
      } else {
        idExpr.setCodegenValue(localVar);
      }
    } else {
      llvm::GlobalVariable* globalVar = globalVariables[idExpr.getId()];
      if (globalVar == nullptr) {
        llvm::errs() << "Unknown variable: " << idExpr.getId() << "\n";
        return;
      }
      llvm::Type* varType = globalVar->getValueType();
      if (globalVariableTypes[idExpr.getId()] == "str" ||
          globalVariableTypes[idExpr.getId()] == "int" ||
          globalVariableTypes[idExpr.getId()] == "bool") {
        llvm::Value* globalVarVal =
            builder->CreateLoad(varType, globalVar, "global_var_val");
        idExpr.setCodegenValue(globalVarVal);
      } else {
        idExpr.setCodegenValue(globalVar);
      }
    }
  }
}

void LLVMCodeGenVisitor::visitCallExpr(const CallExprAST& callExpr) {
  for (auto& arg : callExpr.getArgs()) {
    arg->accept(*this);
  }
  llvm::Function* calleeFunc = nullptr;
  bool isConstructorCall = false;
  if (auto callee = llvm::dyn_cast<IdExprAST>(callExpr.getCallee())) {
    if (callee->getId() == "print") {
      auto& arg = callExpr.getArgs().front();
      llvm::Value* argVal = arg->getCodegenValue();
      std::string argType = arg->getTypeInfo();

      if (argType == "str") {
        llvm::Function* putsFunc = module->getFunction("puts");
        builder->CreateCall(putsFunc, {argVal}, "print_call");
      } else if (argType == "int") {
        llvm::Constant* fmtStr = getOrCreateGlobalFmtStr("%d\n", ".fmt_int");
        llvm::Function* printfFunc = module->getFunction("printf");
        llvm::Value* intVal = argVal;
        builder->CreateCall(printfFunc, {fmtStr, intVal}, "print_call");
      } else if (argType == "bool") {
        llvm::Constant* trueStr = getOrCreateGlobalFmtStr("True", ".fmt_true");
        llvm::Constant* falseStr =
            getOrCreateGlobalFmtStr("False", ".fmt_false");
        llvm::Function* putsFunc = module->getFunction("puts");
        llvm::Value* boolCond =
            builder->CreateICmpEQ(argVal, llvm::ConstantInt::getTrue(*context));
        llvm::Value* strToPrint =
            builder->CreateSelect(boolCond, trueStr, falseStr);
        builder->CreateCall(putsFunc, {strToPrint}, "print_call");
      } else {
        llvm::Function* putsFunc = module->getFunction("puts");
        builder->CreateCall(putsFunc, {argVal}, "print_call");
      }
      return;
    } else if (auto classPtr = getClassByName(callee->getId().str())) {
      isConstructorCall = true;
      size_t structSize = module->getDataLayout().getTypeAllocSize(
          llvmClass(callee->getId().str()));
      llvm::Value* mallocSize =
          llvm::ConstantInt::get(*context, llvm::APInt(32, structSize));
      calleeFunc = module->getFunction("malloc");
      llvm::Value* mallocCall =
          builder->CreateCall(calleeFunc, mallocSize, "malloc_call");
      llvm::Value* bitcast = builder->CreateBitCast(
          mallocCall, llvmClass(callee->getId().str())->getPointerTo());

      llvm::Value* vtablePtr = classToVTable.at(classPtr).getGlobalVTableVal();
      builder->CreateStore(vtablePtr, bitcast);

      const auto& classAttributeMetadata = classFieldGEPMap.at(classPtr);
      // set default field values for bitcast above (raw pointer casted to the
      // class type)
      for (const auto& attributeData : classAttributeMetadata) {
        const auto& attributeGEP = attributeData.second.first;
        const VarDefAST* varDef = attributeData.second.second;
        llvm::Value* initialVal = varDef->getLiteral()->getCodegenValue();
        std::string attributeTypeName =
            varDef->getTypedVar()->getType()->getTypeName();
        llvm::Type* fieldType = llvmTypeOrClassPtrType(attributeTypeName);

        llvm::Value* fieldPtr = builder->CreateGEP(llvmClass(classPtr), bitcast,
                                                   attributeGEP, "field_ptr");

        if (initialVal && initialVal->getType() == fieldType) {
          builder->CreateStore(initialVal, fieldPtr);
        }
      }

      callExpr.setCodegenValue(bitcast);
    } else {
      // global function call
      calleeFunc = module->getFunction(callee->getId());
    }
  }
  if (isConstructorCall) {
    // TODO: create constructor functions (to initialize attributes) and call it
    return;
  }
  assert(calleeFunc && "Function could not be found");
  std::vector<llvm::Value*> args;
  for (const auto& arg : callExpr.getArgs()) {
    llvm::Value* argVal = arg->getCodegenValue();
    if (argVal == nullptr) {
      llvm::errs() << "Unknown argument";
      return;
    }
    args.push_back(argVal);
  }
  llvm::Value* funcCall = builder->CreateCall(calleeFunc, args);
  callExpr.setCodegenValue(funcCall);
}

void LLVMCodeGenVisitor::visitVarDef(const VarDefAST& varDef) {
  varDef.getLiteral()->accept(*this);
  if (currentClass != nullptr) {
    if (currentFunction == nullptr) {
      // class var def (struct member)
      const std::string typeName =
          varDef.getTypedVar()->getType()->getTypeName();
      llvm::Type* varType = llvmTypeOrClassPtrType(typeName);
      llvm::Constant* defaultValue = llvmDefaultValue(typeName);
      varDef.setCodegenValue(defaultValue);
    } else {
      // class method var def (stack var)
      const std::string typeName =
          varDef.getTypedVar()->getType()->getTypeName();
      llvm::Type* varType = llvmTypeOrClassPtrType(typeName);
      llvm::AllocaInst* alloca = builder->CreateAlloca(
          varType, nullptr, varDef.getTypedVar()->getId());
      llvm::Constant* initialVal = llvmLiteralValue(*varDef.getLiteral());
      builder->CreateStore(initialVal, alloca);
      localVariables[varDef.getTypedVar()->getId()] = alloca;
      localVariableType[varDef.getTypedVar()->getId()] =
          llvm::StringRef(varDef.getTypedVar()->getType()->getTypeName());
    }
  } else {
    if (currentFunction == nullptr) {
      // global var def
      const std::string typeName =
          varDef.getTypedVar()->getType()->getTypeName();
      llvm::Type* varType = llvmTypeOrClassPtrType(typeName);
      llvm::Constant* defaultValue = llvmDefaultValue(typeName);
      llvm::Constant* initialVal = llvmLiteralValue(*varDef.getLiteral());
      llvm::GlobalVariable* globalVar = new llvm::GlobalVariable(
          *module, varType, false, llvm::GlobalValue::ExternalLinkage,
          initialVal, varDef.getTypedVar()->getId().str());

      globalVariables[varDef.getTypedVar()->getId()] = globalVar;
      globalVariableTypes[varDef.getTypedVar()->getId()] = typeName;
    } else {
      // global function var def
      const std::string typeName =
          varDef.getTypedVar()->getType()->getTypeName();
      llvm::Type* varType = llvmTypeOrClassPtrType(typeName);
      llvm::AllocaInst* alloca = builder->CreateAlloca(
          varType, nullptr, varDef.getTypedVar()->getId());
      llvm::Constant* initialVal = llvmLiteralValue(*varDef.getLiteral());
      builder->CreateStore(initialVal, alloca);
      localVariables[varDef.getTypedVar()->getId()] = alloca;
      localVariableType[varDef.getTypedVar()->getId()] =
          llvm::StringRef(varDef.getTypedVar()->getType()->getTypeName());
    }
  }
}

void LLVMCodeGenVisitor::visitSimpleStmtAssign(
    const SimpleStmtAssignAST& simpleStmtAssign) {
  simpleStmtAssign.getRhs()->accept(*this);
  llvm::Value* rhsValue = simpleStmtAssign.getRhs()->getCodegenValue();

  auto rhsType = simpleStmtAssign.getRhs()->getTypeInfo();
  if (auto rhsId = llvm::dyn_cast<IdExprAST>(simpleStmtAssign.getRhs())) {
    if (llvmClass(rhsType)) {
      llvm::Type* fieldType = llvmTypeOrClassPtrType(rhsType);
      rhsValue =
          builder->CreateLoad(fieldType, rhsValue, "field_val_loaded_id");
    }
    // If rhs is attrib access then load it, as it only the getelementptr
    // instruction
  } else if (auto binaryRhs =
                 llvm::dyn_cast<BinaryExprAST>(simpleStmtAssign.getRhs())) {
    if (binaryRhs->getOp() == TokenKind::kAttrAccessOp) {
      if (auto rhsRhsId = llvm::dyn_cast<IdExprAST>(binaryRhs->getRhs())) {
        auto rhsLhsType = binaryRhs->getLhs()->getTypeInfo();
        if (llvmClass(rhsLhsType)) {
          std::string fieldTypeName = binaryRhs->getTypeInfo();
          if (llvmClass(fieldTypeName)) {
            llvm::Type* fieldType = llvmTypeOrClassPtrType(fieldTypeName);
            rhsValue = builder->CreateLoad(fieldType, rhsValue,
                                           "field_val_loaded_attrib_acc");
          }
        }
      }
    }
  }
  for (const auto& varTarget : simpleStmtAssign.getTargets()) {
    if (auto idExpr = llvm::dyn_cast<IdExprAST>(varTarget.get())) {
      llvm::Value* var = lookupVariable(idExpr->getId());
      builder->CreateStore(rhsValue, var);
    } else if (auto binaryExpr =
                   llvm::dyn_cast<BinaryExprAST>(varTarget.get())) {
      if (auto rhs = llvm::dyn_cast<IdExprAST>(binaryExpr->getRhs())) {
        binaryExpr->getLhs()->accept(*this);
        std::string instanceType = binaryExpr->getLhs()->getTypeInfo();
        auto classPtr = getClassByName(instanceType);
        auto classType = llvmClass(instanceType);
        llvm::Value* instancePtr = builder->CreateLoad(
            llvmClass(instanceType)->getPointerTo(),
            binaryExpr->getLhs()->getCodegenValue(), "current_instance_ptr");
        const auto& attributeGEP =
            classFieldGEPMap.at(classPtr)[rhs->getId().str()].first;
        llvm::Value* fieldGEP = builder->CreateGEP(
            llvmClass(instanceType), instancePtr, attributeGEP, "field_gep");
        builder->CreateStore(rhsValue, fieldGEP);
      }
    }
  }
}

void LLVMCodeGenVisitor::visitSimpleStmtExpr(
    const SimpleStmtExprAST& simpleStmtExpr) {}

std::vector<llvm::BasicBlock*>
LLVMCodeGenVisitor::getUnterminatedBlocks(llvm::Function* func) {
  auto isExcluded = [&](llvm::BasicBlock* block) {
    return std::find(excludeBlockStack.begin(), excludeBlockStack.end(),
                     block) != excludeBlockStack.end();
  };
  std::vector<llvm::BasicBlock*> unterminated;
  for (auto& block : *func) {
    if (!block.getTerminator() && !isExcluded(&block)) {
      unterminated.push_back(&block);
    }
  }
  return unterminated;
}

void LLVMCodeGenVisitor::visitStmtIf(const StmtIfAST& stmtIf) {
  stmtIf.getCondition()->accept(*this);
  llvm::Value* condVal = stmtIf.getCondition()->getCodegenValue();
  if (!condVal) {
    llvm::errs() << "Unknown condition in if statement\n";
    return;
  }

  llvm::Function* currentFunc = builder->GetInsertBlock()->getParent();
  llvm::BasicBlock* thenBlock =
      llvm::BasicBlock::Create(*context, "if_then", currentFunc);
  llvm::BasicBlock* elseBlock = nullptr;
  llvm::BasicBlock* mergeBlock =
      llvm::BasicBlock::Create(*context, "if_merge", currentFunc);
  excludeBlockStack.push_back(mergeBlock);

  if (!stmtIf.getElseBody().empty()) {
    elseBlock = llvm::BasicBlock::Create(*context, "if_else", currentFunc);
    builder->CreateCondBr(condVal, thenBlock, elseBlock);
  } else {
    builder->CreateCondBr(condVal, thenBlock, mergeBlock);
  }

  excludeBlockStack.push_back(elseBlock);
  // THEN branch
  builder->SetInsertPoint(thenBlock);
  for (const auto& stmt : stmtIf.getBody()) {
    stmt->accept(*this);
  }
  excludeBlockStack.pop_back();

  auto thenUnterminated = getUnterminatedBlocks(currentFunc);

  // ELSE branch (if present)
  std::vector<llvm::BasicBlock*> elseUnterminated;
  if (elseBlock) {
    builder->SetInsertPoint(elseBlock);
    for (const auto& stmt : stmtIf.getElseBody()) {
      stmt->accept(*this);
    }
    elseUnterminated = getUnterminatedBlocks(currentFunc);
  }

  // Only create branches to merge if needed
  bool needMerge =
      !thenUnterminated.empty() || !elseUnterminated.empty() || !elseBlock;
  if (needMerge) {
    for (auto* block : thenUnterminated) {
      builder->SetInsertPoint(block);
      if (!block->getTerminator())
        builder->CreateBr(mergeBlock);
    }

    for (auto* block : elseUnterminated) {
      builder->SetInsertPoint(block);
      if (!block->getTerminator())
        builder->CreateBr(mergeBlock);
    }
    builder->SetInsertPoint(mergeBlock);
    excludeBlockStack.pop_back();
    // Emit code for after the if-statement here, if any.
  }
}

void LLVMCodeGenVisitor::visitStmtWhile(const StmtWhileAST& stmtWhile) {
  llvm::Function* currentFunc = builder->GetInsertBlock()->getParent();

  llvm::BasicBlock* whileCond =
      llvm::BasicBlock::Create(*context, "while_cond", currentFunc);
  llvm::BasicBlock* whileBody =
      llvm::BasicBlock::Create(*context, "while_body", currentFunc);
  llvm::BasicBlock* whileMerge =
      llvm::BasicBlock::Create(*context, "while_merge", currentFunc);

  // Branch to condition block
  builder->CreateBr(whileCond);

  // Emit condition block
  builder->SetInsertPoint(whileCond);
  stmtWhile.getCondition()->accept(*this);
  llvm::Value* condVal = stmtWhile.getCondition()->getCodegenValue();
  if (!condVal) {
    llvm::errs() << "Unknown condition in while statement\n";
    return;
  }

  // Branch based on condition
  builder->CreateCondBr(condVal, whileBody, whileMerge);

  // Emit body block
  builder->SetInsertPoint(whileBody);
  excludeBlockStack.push_back(whileMerge);
  for (const auto& stmt : stmtWhile.getBody()) {
    stmt->accept(*this);
  }

  // Get any unterminated blocks in the body
  auto unterminated = getUnterminatedBlocks(currentFunc);

  // Add branches back to condition for all unterminated blocks
  for (auto* block : unterminated) {
    builder->SetInsertPoint(block);
    if (!block->getTerminator()) {
      builder->CreateBr(whileCond);
    }
  }

  excludeBlockStack.pop_back();

  // Set insert point to merge block for code after the loop
  builder->SetInsertPoint(whileMerge);
}

void LLVMCodeGenVisitor::visitSimpleStmtReturn(
    const SimpleStmtReturnAST& simpleStmtReturn) {
  if (simpleStmtReturn.getExpr()) {
    simpleStmtReturn.getExpr()->accept(*this);
    llvm::Value* retValue = simpleStmtReturn.getExpr()->getCodegenValue();
    builder->CreateRet(retValue);
  } else {
    builder->CreateRet(
        llvmDefaultValue(currentFunction->getReturnType()->getTypeName()));
  }
}

llvm::Value* LLVMCodeGenVisitor::lookupVariable(llvm::StringRef varName) {
  llvm::Value* localVar = localVariables[varName];
  if (localVar)
    return localVar;
  return globalVariables[varName];
}

void LLVMCodeGenVisitor::visitTypedVar(const TypedVarAST& typedVar) {}

void LLVMCodeGenVisitor::createBuiltinFuncDecl(
    const std::string& funcName, const std::string& returnType,
    const std::vector<std::string>& argTypes, bool isVarArg) const {
  std::vector<llvm::Type*> llvmArgTypes;
  for (const auto& arg : argTypes) {
    llvmArgTypes.push_back(llvmType(arg));
  }

  llvm::Type* llvmReturnType = llvmType(returnType);
  createBuiltinFuncDecl(funcName, llvmReturnType, llvmArgTypes, isVarArg);
}

void LLVMCodeGenVisitor::createBuiltinFuncDecl(
    const std::string& funcName, llvm::Type* returnType,
    const std::vector<llvm::Type*>& llvmArgTypes, bool isVarArg) const {
  llvm::FunctionType* funcType =
      llvm::FunctionType::get(returnType, llvmArgTypes, isVarArg);
  module->getOrInsertFunction(funcName, funcType);
}

llvm::Type* LLVMCodeGenVisitor::llvmType(std::string typeName) const {
  if (typeName == "<None>")
    return builder->getVoidTy();
  if (typeName == "int")
    return llvm::Type::getInt32Ty(*context);
  if (typeName == "str")
    return llvm::Type::getInt8PtrTy(*context);
  if (typeName == "bool")
    return llvm::Type::getInt1Ty(*context);
  return nullptr;
}

llvm::Constant*
LLVMCodeGenVisitor::llvmDefaultValue(const std::string& typeName) {
  if (typeName == "<None>")
    return nullptr;
  if (typeName == "int")
    return llvm::ConstantInt::get(*context, llvm::APInt(32, 0));
  if (typeName == "str")
    return llvm::ConstantPointerNull::get(llvm::Type::getInt8PtrTy(*context));
  if (typeName == "bool")
    return llvm::ConstantInt::getFalse(*context);
  return llvm::ConstantPointerNull::get(
      llvmTypeOrClassPtrType(typeName)->getPointerTo());
}

llvm::Constant*
LLVMCodeGenVisitor::llvmLiteralValue(const LiteralAST& literal) {
  if (auto litNum = llvm::dyn_cast<LiteralNumberAST>(&literal)) {
    return llvm::ConstantInt::getSigned(llvm::Type::getInt32Ty(*context),
                                        litNum->getNumber());
  } else if (auto litStr = llvm::dyn_cast<LiteralStringAST>(&literal)) {
    const std::string& str = litStr->getStr().str();

    llvm::GlobalVariable* globalString = nullptr;
    auto it = stringLiteralMap.find(str);
    if (it != stringLiteralMap.end()) {
      globalString = it->second;
    } else {
      llvm::Constant* stringConstant =
          llvm::ConstantDataArray::getString(*context, str, true);

      globalString = new llvm::GlobalVariable(
          *module, stringConstant->getType(), true,
          llvm::GlobalValue::PrivateLinkage, stringConstant, ".str");

      stringLiteralMap[str] = globalString;
    }

    llvm::Constant* zero =
        llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context), 0);
    llvm::Constant* indices[] = {zero, zero};
    llvm::Constant* strPtr = llvm::ConstantExpr::getGetElementPtr(
        globalString->getValueType(), globalString, indices, true);
    return strPtr;
  } else if (llvm::isa<LiteralTrueAST>(literal)) {
    return llvm::ConstantInt::getTrue(*context);
  } else if (llvm::isa<LiteralFalseAST>(literal)) {
    return llvm::ConstantInt::getFalse(*context);
  } else if (llvm::isa<LiteralNoneAST>(literal)) {
    return llvm::ConstantPointerNull::get(
        llvmTypeOrClassPtrType("object")->getPointerTo());
  }
}

llvm::Constant*
LLVMCodeGenVisitor::getOrCreateGlobalFmtStr(const std::string& str,
                                            const std::string& name) {
  auto it = stringLiteralMap.find(str);
  if (it != stringLiteralMap.end())
    return it->second;
  llvm::Constant* strConst =
      llvm::ConstantDataArray::getString(*context, str, true);
  auto* globalStr = new llvm::GlobalVariable(*module, strConst->getType(), true,
                                             llvm::GlobalValue::PrivateLinkage,
                                             strConst, name);
  llvm::Constant* zero =
      llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context), 0);
  llvm::Constant* indices[] = {zero, zero};
  llvm::Constant* strPtr = llvm::ConstantExpr::getGetElementPtr(
      globalStr->getValueType(), globalStr, indices, true);
  stringLiteralMap[str] = globalStr;
  return strPtr;
}

llvm::Type*
LLVMCodeGenVisitor::llvmTypeOrClassPtrType(const std::string& typeName) {
  llvm::Type* type = llvmType(typeName);
  if (type == nullptr) {
    type = llvmClass(typeName)->getPointerTo();
  }
  return type;
}

inline llvm::StructType*
LLVMCodeGenVisitor::llvmClass(const std::string& className) {
  return llvmClass(getClassByName(className));
}

inline llvm::StructType*
LLVMCodeGenVisitor::llvmClass(const ClassAST* classPtr) {
  auto it = classToStructType.find(classPtr);
  if (it != classToStructType.end()) {
    return it->second;
  }
  return nullptr;
}

const ClassAST* LLVMCodeGenVisitor::getClassByName(std::string name) const {
  if (name == "self") {
    name = currentClass->getId();
  }
  return programAST->GetClassPtr(name);
}

inline llvm::Function*
LLVMCodeGenVisitor::llvmFunc(const FunctionAST* function) {
  return functions.at(function);
}

/***********************************/
/* VirtualTable                    */
/***********************************/

void VirtualTable::createVTable(
    llvm::Module* module, const std::vector<llvm::Constant*>& vtableFuncs) {

  std::vector<llvm::Type*> vtableMethodTypes;
  vtableMethodTypes.reserve(vtableFuncs.size());

  for (auto fn : vtableFuncs)
    vtableMethodTypes.push_back(fn->getType());

  virtualTableStructType->setBody(vtableMethodTypes);

  llvm::GlobalVariable* vtableVar = new llvm::GlobalVariable(
      *module, virtualTableStructType, true, llvm::GlobalValue::ExternalLinkage,
      llvm::ConstantStruct::get(virtualTableStructType, vtableFuncs),
      classAST->getId().str() + "-vtbl.chocopy");

  funcs = vtableFuncs;
  globalVTableVal = vtableVar;
}

const std::vector<llvm::Constant*>& VirtualTable::getFuncs() const {
  return funcs;
}

llvm::GlobalValue* VirtualTable::getGlobalVTableVal() const {
  return globalVTableVal;
}

size_t VirtualTable::getVTableIndex(llvm::Constant* llvmFunc) const {
  auto it = std::find(funcs.begin(), funcs.end(), llvmFunc);
  assert(it != funcs.end() && "Function not found in vtable!\n");
  if (it != funcs.end())
    return std::distance(funcs.begin(), it);
}

} // namespace chocopy
