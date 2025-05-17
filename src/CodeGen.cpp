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

  // TODO: temp remove
  for (const auto& classEntry : classFieldGEPMap) {
    const ClassAST* classPtr = classEntry.first;
    std::cout << "Class: " << classPtr->getId().str() << std::endl;
    for (const auto& attrEntry : classEntry.second) {
      const auto& gepIndices = attrEntry.second.first;
      std::cout << "  Attribute: " << attrEntry.first << " | Indices: [";
      for (size_t i = 0; i < gepIndices.size(); ++i) {
        auto* constInt = llvm::dyn_cast<llvm::ConstantInt>(gepIndices[i]);
        if (constInt) {
          std::cout << constInt->getSExtValue();
        } else {
          std::cout << "<not int>";
        }
        if (i + 1 < gepIndices.size())
          std::cout << ", ";
      }
      std::cout << "]" << std::endl;
    }
  }

  for (auto& clazz : program.getClassDefs()) {
    currentClass = const_cast<ClassAST*>(clazz.get());
    clazz->accept(*this);
    currentClass = nullptr;
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
    builder->CreateStore(&param, alloca);
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
      llvm::Type::getInt64Ty(*context), literalNumber.getNumber());
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
  llvm::Value* stringConstant = llvm::ConstantDataArray::getString(
      module->getContext(), literalString.getStr());

  llvm::GlobalVariable* globalString = new llvm::GlobalVariable(
      *module, stringConstant->getType(), true,
      llvm::GlobalValue::PrivateLinkage,
      llvm::cast<llvm::Constant>(stringConstant), ".str");

  llvm::Value* stringPtr = builder->CreateConstGEP1_64(
      stringConstant->getType(), globalString, 0, ".str_ptr");

  literalString.setCodegenValue(stringPtr);
}

void LLVMCodeGenVisitor::visitLiteralNone(const LiteralNoneAST& literalNone) {
  llvm::Value* codegenValue = nullptr;
  literalNone.setCodegenValue(codegenValue);
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
          llvmClass(instanceType), instancePtr, 0, "vtable_ptr_ptr");
      llvm::Value* vtablePtr = builder->CreateLoad(
          getVTable(instanceType).GetVTStructType()->getPointerTo(),
          vtablePtrPtr, "vtable_ptr");

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
      llvm::Value* instancePtr = builder->CreateLoad(
          llvmClass(instanceType)->getPointerTo(),
          binaryExpr.getLhs()->getCodegenValue(), "current_instance_ptr");

      const auto& attributeGEP =
          classFieldGEPMap.at(currentClass)[rhs->getId().str()].first;
      llvm::Value* fieldPtr = builder->CreateGEP(
          llvmClass(instanceType), instancePtr, attributeGEP, "field_ptr");

      llvm::Value* fieldVal =
          builder->CreateLoad(fieldPtr->getType(), fieldPtr, "field_val");
      binaryExpr.setCodegenValue(fieldVal);
    }
    return;
  }
  }
}

void LLVMCodeGenVisitor::visitIdExpr(const IdExprAST& idExpr) {
  if (currentClass == nullptr && currentFunction == nullptr) {
    idExpr.setCodegenValue(globalVariables[idExpr.getId()]);
  } else if (currentClass != nullptr && currentFunction != nullptr) {
    idExpr.setCodegenValue(localVariables[idExpr.getId()]);
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
      calleeFunc = module->getFunction("puts");
      if (!calleeFunc) {
        llvm::errs() << "Unknown function referenced 'printf'\n";
        return;
      }
    } else if (auto classPtr = getClassByName(callee->getId().str())) {
      isConstructorCall = true;
      size_t structSize = module->getDataLayout().getTypeAllocSize(
          llvmClass(callee->getId().str()));
      llvm::Value* mallocSize =
          llvm::ConstantInt::get(*context, llvm::APInt(32, structSize));
      calleeFunc = module->getFunction("malloc");
      llvm::Value* mallocCall = builder->CreateCall(calleeFunc, mallocSize);
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
        llvm::Type* fieldType = llvmTypeOrClassPtrType(
            varDef->getTypedVar()->getType()->getTypeName());

        llvm::Value* fieldPtr = builder->CreateGEP(llvmClass(classPtr), bitcast,
                                                   attributeGEP, "field_ptr");

        if (initialVal && initialVal->getType() == fieldType) {
          builder->CreateStore(initialVal, fieldPtr);
        }
      }

      callExpr.setCodegenValue(bitcast);
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
  builder->CreateCall(calleeFunc, args);
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
    }
  } else {
    if (currentFunction == nullptr) {
      // global var def
      const std::string typeName =
          varDef.getTypedVar()->getType()->getTypeName();
      llvm::Type* varType = llvmTypeOrClassPtrType(typeName);
      llvm::Constant* defaultValue = llvmDefaultValue(typeName);
      llvm::GlobalVariable* globalVar = new llvm::GlobalVariable(
          *module, varType, false, llvm::GlobalValue::ExternalLinkage,
          defaultValue, varDef.getTypedVar()->getId().str());

      globalVariables[varDef.getTypedVar()->getId()] = globalVar;
    } else {
      // global function var def
    }
  }
}

void LLVMCodeGenVisitor::visitSimpleStmtAssign(
    const SimpleStmtAssignAST& simpleStmtAssign) {
  simpleStmtAssign.getRhs()->accept(*this);
  llvm::Value* rhsValue = simpleStmtAssign.getRhs()->getCodegenValue();
  for (const auto& varTarget : simpleStmtAssign.getTargets()) {
    if (auto idExpr = llvm::dyn_cast<IdExprAST>(varTarget.get())) {
      llvm::Value* var = lookupVariable(idExpr->getId());
      builder->CreateStore(rhsValue, var);
    }
  }
}

void LLVMCodeGenVisitor::visitSimpleStmtExpr(
    const SimpleStmtExprAST& simpleStmtExpr) {}

void LLVMCodeGenVisitor::visitSimpleStmtReturn(
    const SimpleStmtReturnAST& simpleStmtReturn) {
  simpleStmtReturn.getExpr()->accept(*this);
  llvm::Value* retValue = simpleStmtReturn.getExpr()->getCodegenValue();
  builder->CreateRet(retValue);
}

llvm::Value* LLVMCodeGenVisitor::lookupVariable(llvm::StringRef varName) {
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
    return llvm::ConstantInt::get(*context, llvm::APInt(64, 0));
  if (typeName == "str")
    return llvm::ConstantPointerNull::get(llvm::Type::getInt8PtrTy(*context));
  if (typeName == "bool")
    return llvm::ConstantInt::getFalse(*context);
  return llvm::ConstantPointerNull::get(
      llvmTypeOrClassPtrType(typeName)->getPointerTo());
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
  return classToStructType.at(classPtr);
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

  module->getOrInsertGlobal(classAST->getId().str() + "-vtbl.chocopy",
                            virtualTableStructType);

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
