#include <filesystem>
#include <iostream>

#include "CodeGen.h"

namespace chocopy {

LLVMCodeGenVisitor::LLVMCodeGenVisitor(ProgramAST* program,
                                       llvm::StringRef programPath)
    : context(std::make_unique<llvm::LLVMContext>()),
      builder(std::make_unique<llvm::IRBuilder<>>(*context)),
      programAST(program), programPath(programPath) {
  scopeManager = std::make_unique<ScopeManager>();
}

LLVMCodeGenVisitor::~LLVMCodeGenVisitor() {}

const FunctionAST* LLVMCodeGenVisitor::currentFunction() const {
  return functionStack.empty() ? nullptr : functionStack.top();
}

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
  createBuiltinFuncDecl("strconcat", "str", {"str", "str"});
  createBuiltinFuncDecl("stridx", "str", {"str", "int"});
  createBuiltinFuncDecl("strlength", "int", {"str"});
  createBuiltinFuncDecl("runtime_check", "int",
                        {"int", "str", "str", "int", "int"});

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
  functionStack.push(const_cast<FunctionAST*>(&func));
  llvm::Function* llvmFunction = llvmFunc(&func);

  llvm::BasicBlock* entry =
      llvm::BasicBlock::Create(*context, "entrypoint", llvmFunction);
  builder->SetInsertPoint(entry);

  scopeManager->pushScope();

  for (auto& param : llvmFunction->args()) {
    int paramIndex = param.getArgNo();
    llvm::Type* paramType =
        llvmFunction->getFunctionType()->getParamType(paramIndex);
    llvm::AllocaInst* alloca = builder->CreateAlloca(
        paramType, nullptr, func.getArgs().at(paramIndex)->getId());
    auto argName = func.getArgs().at(paramIndex)->getId();
    scopeManager->addVar(func.getArgs().at(paramIndex)->getId().str(), alloca,
                         (currentFunction()),
                         func.getArgs()[paramIndex]->getType()->getTypeName());
    builder->CreateStore(&param, alloca);
  }

  for (auto& nonlocalVar : func.getNonlocalDecls()) {
    nonlocalDeclns[&func] = nonlocalVar;
  }

  for (auto& globalVar : func.getGlobalDecls()) {
    globalDeclns[&func] = globalVar;
  }

  for (auto& localVar : func.getVarDefs()) {
    localVar->accept(*this);
  }

  for (auto& nestedFunc : func.getFuncDefs()) {
    functionStack.push(nestedFunc.get());
    createReferenceEnvType(nestedFunc.get());
    nestedFunc->accept(*this);
    functionStack.pop();
  }

  builder->SetInsertPoint(entry);

  for (const auto& stmt : func.getBody()) {
    stmt->accept(*this);
  }

  if (!builder->GetInsertBlock()->getTerminator()) {
    builder->CreateRet(llvmDefaultValue(func.getReturnType()->getTypeName()));
  }
  functionStack.pop();
  scopeManager->popScope();
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

  scopeManager->pushScope();
  for (const auto& stmt : stmts) {
    stmt->accept(*this);
  }
  scopeManager->popScope();

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

void LLVMCodeGenVisitor::visitListLiteralExpr(
    const ListLiteralExprAST& listLiteralExpr) {
  std::vector<llvm::Value*> elements;
  size_t dimension = listLiteralExpr.getListDimension();

  // Recursive helper to process each level
  std::function<llvm::Value*(const ListLiteralExprAST&, size_t)> buildList;
  buildList = [&](const ListLiteralExprAST& node,
                  size_t level) -> llvm::Value* {
    std::vector<llvm::Value*> items;
    for (const auto& element : node.getElements()) {
      if (auto* inner = llvm::dyn_cast<ListLiteralExprAST>(element.get())) {
        items.push_back(buildList(*inner, level + 1));
      } else {
        element->accept(*this);
        items.push_back(element->getCodegenValue());
      }
    }
    auto& elements = node.getElements();
    llvm::Type* elemType = llvmTypeOrClassPtrType(
        !elements.empty() ? elements[0]->getTypeInfo() : "<Empty>");
    size_t arraySize =
        module->getDataLayout().getTypeAllocSize(elemType) * items.size();
    llvm::Value* arrayMallocSize =
        llvm::ConstantInt::get(*context, llvm::APInt(32, arraySize));
    llvm::Value* arrayMallocCall = builder->CreateCall(
        module->getFunction("malloc"), arrayMallocSize, "arr_malloc_call");
    // Store each item in the allocated array
    for (size_t i = 0; i < items.size(); ++i) {
      llvm::Value* itemPtr = builder->CreateGEP(
          elemType, arrayMallocCall,
          llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context), i),
          "item_ptr");
      builder->CreateStore(items[i], itemPtr);
    }
    // Set the length of the array
    llvm::Value* length =
        llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context), items.size());
    // Create a struct to hold the array and its length
    llvm::StructType* listStructType = llvm::StructType::create(
        *context, {llvm::Type::getInt32Ty(*context), elemType->getPointerTo()},
        "ListStruct");
    size_t arrayStructSize =
        module->getDataLayout().getTypeAllocSize(listStructType);
    llvm::Value* arrayStructMallocCall = builder->CreateCall(
        module->getFunction("malloc"),
        llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context),
                               arrayStructSize),
        "array_struct_malloc_call");
    // Store the length and array pointer in the struct
    llvm::Value* lengthPtr = builder->CreateStructGEP(
        listStructType, arrayStructMallocCall, 0, "length_ptr");
    builder->CreateStore(length, lengthPtr);
    llvm::Value* arrPtr = builder->CreateStructGEP(
        listStructType, arrayStructMallocCall, 1, "array_ptr");
    builder->CreateStore(arrayMallocCall, arrPtr);
    // Return the struct pointer
    return arrayStructMallocCall;
  };

  llvm::Value* listVal = buildList(listLiteralExpr, 1);
  listLiteralExpr.setCodegenValue(listVal);
}

void LLVMCodeGenVisitor::visitBinaryExpr(const BinaryExprAST& binaryExpr) {
  switch (binaryExpr.getOp()) {
  case TokenKind::kAttrAccessOp: {
    if (auto rhs = llvm::dyn_cast<CallExprAST>(binaryExpr.getRhs())) {
      const_cast<ExprAST*>(binaryExpr.getLhs())
          ->setAccessKind(AccessKind::DispatchArg);
      binaryExpr.getLhs()->accept(*this);
      std::string instanceType = binaryExpr.getLhs()->getTypeInfo();

      llvm::Value* instancePtr = binaryExpr.getLhs()->getCodegenValue();

      // load unless direct constructor call is used
      if (!llvm::isa<CallExprAST>(binaryExpr.getLhs())) {
        instancePtr = builder->CreateLoad(
            getVTable(instanceType).GetVTStructType()->getPointerTo(),
            binaryExpr.getLhs()->getCodegenValue(), "current_instance_ptr");
      }

      auto funcId = llvm::dyn_cast<IdExprAST>(rhs->getCallee())->getId().str();
      std::string functionName = instanceType + "-" + funcId;
      auto llvmFunc = functionNameToFunc[functionName];

      size_t vTableIndex = getVTable(instanceType).getVTableIndex(llvmFunc);

      for (const auto& arg : rhs->getArgs()) {
        auto type = arg->getTypeInfo();
        arg->accept(*this);
        if (llvmClass(type) || isListType(type)) {
          if (llvm::isa<IdExprAST>(arg.get()) ||
              llvm::isa<BinaryExprAST>(arg.get())) {
            llvm::Value* argVal = arg->getCodegenValue();
            llvm::Value* loadedArgVal = builder->CreateLoad(
                argVal->getType(), argVal, "loaded_arg_val");
            arg->setCodegenValue(loadedArgVal);
          }
        }
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
      if (!llvmClass(fieldTypeName) && !isListType(fieldTypeName)) {
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
    auto lhsType = binaryExpr.getLhs()->getTypeInfo();
    auto rhsType = binaryExpr.getRhs()->getTypeInfo();
    if (lhsType == "str" && rhsType == "str") {
      llvm::Function* strConcatFunc = module->getFunction("strconcat");
      llvm::FunctionType* strConcatFuncType = strConcatFunc->getFunctionType();
      std::vector<llvm::Value*> args = {lhsVal, rhsVal};
      llvm::Value* concatVal = builder->CreateCall(
          strConcatFuncType, strConcatFunc, args, "concat_strings");
      binaryExpr.setCodegenValue(concatVal);
    } else if (lhsType == "int" && rhsType == "int") {
      llvm::Value* sumVal = builder->CreateAdd(lhsVal, rhsVal, "sum_ints");
      binaryExpr.setCodegenValue(sumVal);
    } else if (isListType(lhsType) && isListType(rhsType)) {
      // List concatenation: lhsVal and rhsVal are pointers to list structs
      llvm::StructType* listStructType = llvm::StructType::get(
          *context, {llvm::Type::getInt32Ty(*context),
                     llvm::PointerType::get(*context, 0)});
      llvm::Type* elemType = llvmTypeOrClassPtrType(getInnerType(lhsType));

      // Load list struct pointers
      llvm::Value* lhsListPtr = lhsVal;
      if (auto binExp = llvm::dyn_cast<BinaryExprAST>(binaryExpr.getLhs())) {
        if (binExp->getOp() != TokenKind::kPlus) {
          lhsListPtr = builder->CreateLoad(listStructType->getPointerTo(),
                                           lhsVal, "lhs_list_ptr");
        }
      } else if (!llvm::isa<ListLiteralExprAST>(binaryExpr.getLhs())) {
        lhsListPtr = builder->CreateLoad(listStructType->getPointerTo(), lhsVal,
                                         "lhs_list_ptr");
      }

      llvm::Value* rhsListPtr = rhsVal;
      if (auto binExp = llvm::dyn_cast<BinaryExprAST>(binaryExpr.getRhs())) {
        if (binExp->getOp() != TokenKind::kPlus) {
          rhsListPtr = builder->CreateLoad(listStructType->getPointerTo(),
                                           rhsVal, "rhs_list_ptr");
        }
      } else if (!llvm::isa<ListLiteralExprAST>(binaryExpr.getRhs())) {
        rhsListPtr = builder->CreateLoad(listStructType->getPointerTo(), rhsVal,
                                         "rhs_list_ptr");
      }

      // Get lengths
      llvm::Value* lhsLenPtr = builder->CreateStructGEP(
          listStructType, lhsListPtr, 0, "lhs_len_ptr");
      llvm::Value* lhsLen = builder->CreateLoad(
          llvm::Type::getInt32Ty(*context), lhsLenPtr, "lhs_len");
      llvm::Value* rhsLenPtr = builder->CreateStructGEP(
          listStructType, rhsListPtr, 0, "rhs_len_ptr");
      llvm::Value* rhsLen = builder->CreateLoad(
          llvm::Type::getInt32Ty(*context), rhsLenPtr, "rhs_len");
      llvm::Value* totalLen = builder->CreateAdd(lhsLen, rhsLen, "total_len");

      // Allocate new array
      llvm::Value* allocSize = builder->CreateMul(
          totalLen,
          llvm::ConstantInt::get(
              llvm::Type::getInt32Ty(*context),
              module->getDataLayout().getTypeAllocSize(elemType)),
          "alloc_size");

      llvm::Value* newArrayRaw = builder->CreateCall(
          module->getFunction("malloc"), allocSize, "concat_arr");
      llvm::Value* newArray = builder->CreateBitCast(
          newArrayRaw, elemType->getPointerTo(), "new_array");

      // Get array pointers from both lists
      llvm::Value* lhsArrPtrPtr = builder->CreateStructGEP(
          listStructType, lhsListPtr, 1, "lhs_arr_ptr_ptr");
      llvm::Value* lhsArr = builder->CreateLoad(elemType->getPointerTo(),
                                                lhsArrPtrPtr, "lhs_arr");
      llvm::Value* rhsArrPtrPtr = builder->CreateStructGEP(
          listStructType, rhsListPtr, 1, "rhs_arr_ptr_ptr");
      llvm::Value* rhsArr = builder->CreateLoad(elemType->getPointerTo(),
                                                rhsArrPtrPtr, "rhs_arr");

      // Copy elements from lhsArr to newArray
      llvm::Function* currFunc = builder->GetInsertBlock()->getParent();
      llvm::BasicBlock* copyLhsCond =
          llvm::BasicBlock::Create(*context, "copy_lhs_cond", currFunc);
      llvm::BasicBlock* copyLhsBody =
          llvm::BasicBlock::Create(*context, "copy_lhs_body", currFunc);
      llvm::BasicBlock* copyLhsEnd =
          llvm::BasicBlock::Create(*context, "copy_lhs_end", currFunc);
      llvm::AllocaInst* iAlloca =
          builder->CreateAlloca(llvm::Type::getInt32Ty(*context), nullptr, "i");
      builder->CreateStore(
          llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context), 0), iAlloca);
      builder->CreateBr(copyLhsCond);

      // Copy elements from lhsArr to newArray
      builder->SetInsertPoint(copyLhsCond);
      llvm::Value* iVal = builder->CreateLoad(llvm::Type::getInt32Ty(*context),
                                              iAlloca, "i_val");
      llvm::Value* lhsCond = builder->CreateICmpSLT(iVal, lhsLen, "lhs_cond");
      builder->CreateCondBr(lhsCond, copyLhsBody, copyLhsEnd);
      builder->SetInsertPoint(copyLhsBody);
      llvm::Value* srcPtr =
          builder->CreateGEP(elemType, lhsArr, iVal, "src_ptr");
      llvm::Value* val = builder->CreateLoad(elemType, srcPtr, "val");
      llvm::Value* dstPtr =
          builder->CreateGEP(elemType, newArray, iVal, "dst_ptr");
      builder->CreateStore(val, dstPtr);
      llvm::Value* iNext = builder->CreateAdd(
          iVal, llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context), 1),
          "i_next");
      builder->CreateStore(iNext, iAlloca);
      builder->CreateBr(copyLhsCond);

      // End block for lhs copy
      builder->SetInsertPoint(copyLhsEnd);

      // Copy elements from rhsArr to newArray
      llvm::BasicBlock* copyRhsCond =
          llvm::BasicBlock::Create(*context, "copy_rhs_cond", currFunc);
      llvm::BasicBlock* copyRhsBody =
          llvm::BasicBlock::Create(*context, "copy_rhs_body", currFunc);
      llvm::BasicBlock* copyRhsEnd =
          llvm::BasicBlock::Create(*context, "copy_rhs_end", currFunc);
      llvm::AllocaInst* jAlloca =
          builder->CreateAlloca(llvm::Type::getInt32Ty(*context), nullptr, "j");
      builder->CreateStore(
          llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context), 0), jAlloca);
      builder->CreateBr(copyRhsCond);
      builder->SetInsertPoint(copyRhsCond);
      llvm::Value* jVal = builder->CreateLoad(llvm::Type::getInt32Ty(*context),
                                              jAlloca, "j_val");
      llvm::Value* rhsCond = builder->CreateICmpSLT(jVal, rhsLen, "rhs_cond");
      builder->CreateCondBr(rhsCond, copyRhsBody, copyRhsEnd);
      builder->SetInsertPoint(copyRhsBody);
      llvm::Value* srcPtrR =
          builder->CreateGEP(elemType, rhsArr, jVal, "src_ptr_r");
      llvm::Value* valR = builder->CreateLoad(elemType, srcPtrR, "val_r");
      llvm::Value* dstPtrR = builder->CreateGEP(
          elemType, newArray, builder->CreateAdd(jVal, lhsLen), "dst_ptr_r");
      builder->CreateStore(valR, dstPtrR);
      llvm::Value* jNext = builder->CreateAdd(
          jVal, llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context), 1),
          "j_next");
      builder->CreateStore(jNext, jAlloca);
      builder->CreateBr(copyRhsCond);
      builder->SetInsertPoint(copyRhsEnd);

      // Allocate new list struct
      size_t structSize =
          module->getDataLayout().getTypeAllocSize(listStructType);
      llvm::Value* newListStruct = builder->CreateCall(
          module->getFunction("malloc"),
          llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context), structSize),
          "new_list_struct");
      llvm::Value* newLenPtr = builder->CreateStructGEP(
          listStructType, newListStruct, 0, "new_len_ptr");
      builder->CreateStore(totalLen, newLenPtr);
      llvm::Value* newArrPtr = builder->CreateStructGEP(
          listStructType, newListStruct, 1, "new_arr_ptr");
      builder->CreateStore(newArray, newArrPtr);
      binaryExpr.setCodegenValue(newListStruct);
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
    llvm::Value* rem = builder->CreateSRem(lhsVal, rhsVal, "mod_ints");
    llvm::Value* isNeg =
        builder->CreateICmpSLT(rem, llvm::ConstantInt::get(rem->getType(), 0));
    llvm::Value* adjusted = builder->CreateAdd(rem, rhsVal);
    llvm::Value* finalMod = builder->CreateSelect(isNeg, adjusted, rem);
    binaryExpr.setCodegenValue(finalMod);
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
  case TokenKind::kIndexAccessOp: {
    binaryExpr.getLhs()->accept(*this);
    binaryExpr.getRhs()->accept(*this);
    llvm::Value* lhsVal = binaryExpr.getLhs()->getCodegenValue();
    llvm::Value* rhsVal = binaryExpr.getRhs()->getCodegenValue();
    if (lhsVal == nullptr || rhsVal == nullptr) {
      llvm::errs() << "Unknown operands in '[]' expression\n";
      return;
    }
    if (binaryExpr.getLhs()->getTypeInfo() == "str") {
      llvm::Function* strIdxFunc = module->getFunction("stridx");
      llvm::Value* strIdxVal =
          builder->CreateCall(strIdxFunc, {lhsVal, rhsVal}, "stridx_call");
      binaryExpr.setCodegenValue(strIdxVal);
      return;
    } else {
      // List index access
      llvm::Type* listStructType = llvm::StructType::get(
          *context,
          {llvm::Type::getInt32Ty(*context),
           llvmTypeOrClassPtrType(binaryExpr.getLhs()->getTypeInfo())});
      // Load the list struct pointer
      llvm::Value* listStructPtr = builder->CreateLoad(
          listStructType->getPointerTo(), lhsVal, "list_ptr_val");
      // Get pointer to the array pointer field
      llvm::Value* arrayPtrPtr = builder->CreateStructGEP(
          listStructType, listStructPtr, 1, "array_ptr");
      // Load the actual array pointer
      llvm::Value* arrayPtr = builder->CreateLoad(
          listStructType->getStructElementType(1), arrayPtrPtr, "array_val");
      // Calculate the element pointer using the index
      llvm::Value* elemPtr =
          builder->CreateGEP(llvmTypeOrClassPtrType(binaryExpr.getTypeInfo()),
                             arrayPtr, rhsVal, "elem_ptr");

      llvm::Value* arrayLengthPtr = builder->CreateStructGEP(
          listStructType, listStructPtr, 0, "arr_length_ptr");
      llvm::Value* lengthVal =
          builder->CreateLoad(listStructType->getStructElementType(0),
                              arrayLengthPtr, "length_val");

      llvm::Function* runTimeCheckFn = module->getFunction("runtime_check");

      // Index out of bound check
      llvm::Value* inBounds =
          builder->CreateICmpSLT(rhsVal, lengthVal, "idx_in_bounds");
      llvm::Value* nonNegative = builder->CreateICmpSGE(
          rhsVal, llvm::ConstantInt::get(rhsVal->getType(), 0),
          "idx_non_negative");
      llvm::Value* cond =
          builder->CreateAnd(inBounds, nonNegative, "idx_valid");

      llvm::Constant* fileStr =
          getOrCreateGlobalFmtStr(*(binaryExpr.loc().file), ".src_file");
      llvm::Constant* lineVal = llvm::ConstantInt::get(
          llvm::Type::getInt32Ty(*context), binaryExpr.loc().line);
      llvm::Constant* colVal = llvm::ConstantInt::get(
          llvm::Type::getInt32Ty(*context), binaryExpr.loc().col);

      llvm::Constant* errMsg = getOrCreateGlobalFmtStr(
          "Runtime error: List index out of bounds", ".idx_oob");
      builder->CreateCall(runTimeCheckFn,
                          {cond, errMsg, fileStr, lineVal, colVal});

      if (binaryExpr.getAccessKind() == AccessKind::Write ||
          binaryExpr.getAccessKind() == AccessKind::ListAccess ||
          binaryExpr.getAccessKind() == AccessKind::DispatchArg) {
        binaryExpr.setCodegenValue(elemPtr);
        return;
      }
      // Load the element value
      llvm::Value* elemVal =
          builder->CreateLoad(llvmTypeOrClassPtrType(binaryExpr.getTypeInfo()),
                              elemPtr, "elem_val");
      binaryExpr.setCodegenValue(elemVal);
      return;
    }
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

std::tuple<int, int>
LLVMCodeGenVisitor::calculateClosureDimensions(const FunctionAST* nestedFunc,
                                               const std::string& varName) {
  int staticChainDerefCount = 0;
  int closureGEPDistance = 0;

  const FunctionAST* parentFunc = nestedFunc->getParentFunc();
  while (parentFunc) {

    for (auto& arg : parentFunc->getArgs()) {
      closureGEPDistance++;
      if (arg->getId().str() == varName) {
        return std::make_tuple(staticChainDerefCount, closureGEPDistance);
      }
    }

    for (auto& varDef : parentFunc->getVarDefs()) {
      closureGEPDistance++;
      if (varDef->getTypedVar()->getId().str() == varName) {
        return std::make_tuple(staticChainDerefCount, closureGEPDistance);
      }
    }
    staticChainDerefCount++;
    parentFunc = parentFunc->getParentFunc();
    closureGEPDistance = 0;
  }

  throw std::runtime_error("Closure variable '" + varName +
                           "' could not be found for '" +
                           nestedFunc->getId().str() + "'.");
}

void LLVMCodeGenVisitor::visitIdExpr(const IdExprAST& idExpr) {
  // Check whether this is a global var declaration resolution
  auto currentFn = currentFunction();
  if (currentFn) {
    auto found = globalDeclns.find(currentFn);
    if (found != globalDeclns.end()) {
      if (found->second == idExpr.getId().str()) {
        llvm::GlobalVariable* globalVar = globalVariables[idExpr.getId().str()];
        if (globalVar == nullptr) {
          llvm::errs() << "Unknown global variable: " << idExpr.getId() << "\n";
          return;
        }
        llvm::Type* varType = globalVar->getValueType();
        if (isPrimitiveType(globalVariableTypes[idExpr.getId()])) {
          llvm::Value* globalVarVal =
              builder->CreateLoad(varType, globalVar, "global_var_val");
          idExpr.setCodegenValue(globalVarVal);
        } else {
          idExpr.setCodegenValue(globalVar);
        }
        return;
      }
    }

    if (currentFn->isNestedFunc()) {
      // check for nonlocal declarations
      const VarInfor* varInfo = scopeManager->lookupVar(idExpr.getId());
      if (varInfo) {
        const auto& [_, varDeclaredFn, type, isIterVar] = *varInfo;
        if (varDeclaredFn != currentFn) {
          // lookup the var using ref_env arg
          auto [staticChainDerefCount, varGEPDistance] =
              calculateClosureDimensions(currentFn, idExpr.getId().str());
          const VarInfor* refEnvVarInfo = scopeManager->lookupVar("ref_env");
          if (refEnvVarInfo && refEnvVarInfo->funcPtr == currentFn) {
            // find the actual function with the closure (when deeply nested
            // (more than one level) function accessing a var)
            const FunctionAST* clouseFunc = currentFn;
            // load the first ref_env (closure)
            llvm::Value* refEnvVarAddress = builder->CreateLoad(
                nestedFuncToRefEnvType[clouseFunc]->getPointerTo(),
                refEnvVarInfo->var);
            // static chain dereference
            while (staticChainDerefCount > 0) {
              auto parentRefEnvType = nestedFuncToRefEnvType[clouseFunc];
              // ref_env is assumed to store its parent at index 0
              refEnvVarAddress = builder->CreateStructGEP(
                  parentRefEnvType, refEnvVarAddress, 0, "static_chain_gep");
              refEnvVarAddress = builder->CreateLoad(
                  parentRefEnvType->getPointerTo(), refEnvVarAddress);
              clouseFunc = clouseFunc->getParentFunc();
              staticChainDerefCount--;
            }
            // find the closure type
            auto refEnvStructType = nestedFuncToRefEnvType[clouseFunc];
            // create GEP for the var in ref env
            llvm::Value* refEnvVarGEP =
                builder->CreateStructGEP(refEnvStructType, refEnvVarAddress,
                                         varGEPDistance, "ref_env_var_gep");
            // load var address
            llvm::Value* varLocationLoaded = builder->CreateLoad(
                refEnvStructType->getElementType(varGEPDistance), refEnvVarGEP);
            // load var conditions
            if (isPrimitiveType(type) || (isIterVar && !llvmClass(type))) {
              llvm::Value* loadedRefEnvVal =
                  builder->CreateLoad(llvmTypeOrClassPtrType(type),
                                      varLocationLoaded, "ref_env_loaded_var");
              idExpr.setCodegenValue(loadedRefEnvVal);
            } else {
              idExpr.setCodegenValue(varLocationLoaded);
            }
            return;
          }
        }
      }
    }
  }

  if (currentClass == nullptr && currentFunction() == nullptr) {
    const VarInfor* varInfo = scopeManager->lookupVar(idExpr.getId());
    if (varInfo) {
      const auto& [localVar, _, type, isIterVar] = *varInfo;
      // only load primitive types
      if (isPrimitiveType(type) || (isIterVar && !llvmClass(type))) {
        llvm::Value* localVarVal = builder->CreateLoad(
            static_cast<llvm::AllocaInst*>(localVar)->getAllocatedType(),
            localVar, "local_var_val");
        idExpr.setCodegenValue(localVarVal);
      } else {
        idExpr.setCodegenValue(localVar);
      }
      return;
    }
    llvm::GlobalVariable* globalVar = globalVariables[idExpr.getId()];
    if (globalVar == nullptr) {
      llvm::errs() << "Unknown global variable: " << idExpr.getId() << "\n";
      return;
    }
    llvm::Type* varType = globalVar->getValueType();
    if (isPrimitiveType(globalVariableTypes[idExpr.getId()])) {
      llvm::Value* globalVarVal =
          builder->CreateLoad(varType, globalVar, "global_var_val");
      idExpr.setCodegenValue(globalVarVal);
    } else {
      idExpr.setCodegenValue(globalVar);
    }
  } else if (currentClass != nullptr && currentFunction() != nullptr) {
    if (idExpr.getId() == "self") {
      idExpr.setCodegenValue((*(scopeManager->lookupVar(idExpr.getId()))).var);
    } else {
      const VarInfor* varInfo = scopeManager->lookupVar(idExpr.getId());
      if (!varInfo) {
        llvm::errs() << "Unknown local variable: " << idExpr.getId() << "\n";
        return;
      }
      const auto& [localVar, _, type, isIterVar] = *varInfo;
      // only load primitive types
      if (isPrimitiveType(type) || (isIterVar && !llvmClass(type))) {
        llvm::Value* localVarVal = builder->CreateLoad(
            static_cast<llvm::AllocaInst*>(localVar)->getAllocatedType(),
            localVar, "local_var_val");
        idExpr.setCodegenValue(localVarVal);
      } else {
        idExpr.setCodegenValue(localVar);
      }
    }
  } else if (currentClass == nullptr && currentFunction() != nullptr) {
    const VarInfor* varInfo = scopeManager->lookupVar(idExpr.getId());
    if (varInfo) {
      const auto& [localVar, _, type, isIterVar] = *varInfo;
      // only load primitive types
      if (isPrimitiveType(type) || (isIterVar && !llvmClass(type))) {
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
      if (isPrimitiveType(globalVariableTypes[idExpr.getId()]) ||
          (!llvmClass(globalVariableTypes[idExpr.getId()]))) {
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
    auto type = arg->getTypeInfo();
    arg->accept(*this);
    if (llvmClass(type) || isListType(type)) {
      if (auto idExpr = llvm::dyn_cast<IdExprAST>(arg.get())) {
        llvm::Value* argVal = arg->getCodegenValue();
        llvm::Value* loadedArgVal =
            builder->CreateLoad(argVal->getType(), argVal, "loaded_arg_val");
        arg->setCodegenValue(loadedArgVal);
      }
    }
  }
  llvm::Function* calleeFunc = nullptr;
  bool isConstructorCall = false;
  bool isNestedFuncCall = false;
  bool isNestedFuncRecursiveCall = false;
  const FunctionAST* resolvedNestedFunc = nullptr;
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
    } else if (callee->getId() == "len") {
      auto& arg = callExpr.getArgs().front();
      if (auto binaryExpr = llvm::dyn_cast<BinaryExprAST>(arg.get())) {
        if (binaryExpr->getOp() == TokenKind::kAttrAccessOp) {
          llvm::Value* loadedArgVal =
              builder->CreateLoad(llvmTypeOrClassPtrType(arg->getTypeInfo()),
                                  arg->getCodegenValue(), "loaded_arg_val");
          arg->setCodegenValue(loadedArgVal);
        }
      }
      llvm::Value* listStructPtr = nullptr;
      if (isListType(arg->getTypeInfo())) {
        llvm::StructType* listStructTy =
            llvm::StructType::get(*context,
                                  {llvm::Type::getInt32Ty(*context),
                                   llvmTypeOrClassPtrType(arg->getTypeInfo())},
                                  false);
        llvm::Value* argStoragePtr = arg->getCodegenValue();
        listStructPtr = argStoragePtr;
        if (auto binaryExpr = llvm::dyn_cast<BinaryExprAST>(arg.get())) {
          if (binaryExpr->getOp() == TokenKind::kIndexAccessOp) {
            listStructPtr = builder->CreateLoad(listStructTy->getPointerTo(),
                                                argStoragePtr, "list_ptr_val");
          }
        }
        llvm::Value* lengthPtr = builder->CreateStructGEP(
            listStructTy, listStructPtr, 0, "length_ptr");
        llvm::Value* lengthVal = builder->CreateLoad(
            llvm::Type::getInt32Ty(*context), lengthPtr, "length_val");
        callExpr.setCodegenValue(lengthVal);
        return;
      } else if (arg->getTypeInfo() != "str") {
        llvm::errs() << "len() can only be called on str or list types\n";
        return;
      }
      llvm::Value* argVal = arg->getCodegenValue();
      llvm::Function* strLenFunc = module->getFunction("strlength");
      llvm::Value* strLenVal =
          builder->CreateCall(strLenFunc, {argVal}, "str_len");
      callExpr.setCodegenValue(strLenVal);
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

      // Call the constructor __init__ method in the same class
      // If not found call the parent's __init__ method
      llvm::Function* constructorDunderMethod = nullptr;
      std::string constructorClass = callee->getId().str();
      while (constructorClass != "object") {
        constructorDunderMethod =
            module->getFunction(constructorClass + "-__init__");
        if (constructorDunderMethod) {
          break;
        }
        auto parentClass = getClassByName(constructorClass)->getParentClass();
        if (!parentClass) {
          break;
        }
        constructorClass = parentClass->getId().str();
      }

      if (constructorDunderMethod) {
        std::vector<llvm::Value*> args;
        args.push_back(bitcast);
        for (const auto& arg : callExpr.getArgs()) {
          llvm::Value* argVal = arg->getCodegenValue();
          if (argVal == nullptr) {
            llvm::errs() << "Unknown argument in constructor call\n";
            return;
          }
          args.push_back(argVal);
        }
        builder->CreateCall(constructorDunderMethod, args);
      }

      callExpr.setCodegenValue(bitcast);
    } else {
      // first check for nested function
      auto currentFn = currentFunction();
      if (currentFn) {
        if (currentFn->getId().str() == callee->getId()) {
          calleeFunc = const_cast<llvm::Function*>(nestedFuncs[currentFn]);
          resolvedNestedFunc = currentFn;
          isNestedFuncCall = true;
          isNestedFuncRecursiveCall = true;
        } else {
          // look for child function
          for (auto& innerFunc : currentFn->getFuncDefs()) {
            if (innerFunc->getId().str() == callee->getId()) {
              calleeFunc = nestedFuncs[innerFunc.get()];
              isNestedFuncCall = true;
              resolvedNestedFunc = innerFunc.get();
            }
          }
        }
      }
      // then look for a top level function
      if (calleeFunc == nullptr) {
        calleeFunc = module->getFunction(callee->getId());
      }
    }
  }
  if (isConstructorCall) {
    // TODO: create constructor functions (to initialize attributes) and call it
    return;
  }
  assert(calleeFunc && "Function could not be found");
  std::vector<llvm::Value*> args;
  if (isNestedFuncCall) {
    if (isNestedFuncRecursiveCall) {
      // Pass the ref_env arg (first hidden arg) again as the ref_env to the
      // recursive call
      const VarInfor* refEnvVarInfo = scopeManager->lookupVar("ref_env");
      if (refEnvVarInfo) {
        llvm::StructType* refEnvType =
            nestedFuncToRefEnvType[resolvedNestedFunc];
        llvm::Value* refEnvPtrValue = builder->CreateLoad(
            refEnvType->getPointerTo(), refEnvVarInfo->var, "ref_env_loaded");
        args.push_back(refEnvPtrValue);
      }
    } else {
      // get ref env struct type
      llvm::StructType* refEnvType = nestedFuncToRefEnvType[resolvedNestedFunc];
      size_t refEnvStructSize =
          module->getDataLayout().getTypeAllocSize(refEnvType);
      llvm::Value* mallocSize =
          llvm::ConstantInt::get(*context, llvm::APInt(32, refEnvStructSize));
      llvm::Value* mallocCall = builder->CreateCall(
          module->getFunction("malloc"), mallocSize, "ref_env_malloc_call");

      llvm::Value* refEnvBitcast = builder->CreateBitCast(
          mallocCall, refEnvType->getPointerTo(), "ref_env_bitcast");

      // TODO: just store nullptr for now, this should be the static link for
      // nested closures
      llvm::Value* parentRefEnvPtr = llvmDefaultValue("object");
      if (currentFunction()->isNestedFunc()) {
        const VarInfor* refEnvVarInfo = scopeManager->lookupVar("ref_env");
        if (refEnvVarInfo) {
          llvm::StructType* refEnvType =
              nestedFuncToRefEnvType[resolvedNestedFunc];
          parentRefEnvPtr = builder->CreateLoad(
              refEnvType->getPointerTo(), refEnvVarInfo->var, "ref_env_loaded");
        }
      }
      builder->CreateStore(parentRefEnvPtr, refEnvBitcast);

      const FunctionAST* parentFunc = resolvedNestedFunc->getParentFunc();
      int counterGEP = 1; // assuming index 0 is for static chain

      // store arg locations
      for (auto& arg : parentFunc->getArgs()) {
        const VarInfor* varInfo = scopeManager->lookupVar(arg->getId().str());
        if (varInfo) {
          const auto& [localVar, _, type, isIterVar] = *varInfo;
          llvm::Value* localVarPtrInClosure =
              builder->CreateStructGEP(refEnvType, refEnvBitcast, counterGEP);
          builder->CreateStore(localVar, localVarPtrInClosure);
        }
        counterGEP++;
      }

      // store var locations
      for (auto& varDef : parentFunc->getVarDefs()) {
        const VarInfor* varInfo =
            scopeManager->lookupVar(varDef->getTypedVar()->getId());
        if (varInfo) {
          const auto& [localVar, _, type, isIterVar] = *varInfo;
          llvm::Value* localVarPtr = builder->CreateStructGEP(
              refEnvType, refEnvBitcast, counterGEP, "ref_env_var_gep");
          builder->CreateStore(localVar, localVarPtr);
        }
        counterGEP++;
      }

      // reference env link is the first argumet to the nested function call
      args.push_back(refEnvBitcast);
    }
  }
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
    if (currentFunction() == nullptr) {
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
      scopeManager->addVar(varDef.getTypedVar()->getId(), alloca,
                           currentFunction(),
                           varDef.getTypedVar()->getType()->getTypeName());
    }
  } else {
    if (currentFunction() == nullptr) {
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
      scopeManager->addVar(varDef.getTypedVar()->getId(), alloca,
                           currentFunction(),
                           varDef.getTypedVar()->getType()->getTypeName());
    }
  }
}

void LLVMCodeGenVisitor::visitSimpleStmtAssign(
    const SimpleStmtAssignAST& simpleStmtAssign) {
  simpleStmtAssign.getRhs()->accept(*this);
  llvm::Value* rhsValue = simpleStmtAssign.getRhs()->getCodegenValue();

  auto rhsType = simpleStmtAssign.getRhs()->getTypeInfo();
  if (auto rhsId = llvm::dyn_cast<IdExprAST>(simpleStmtAssign.getRhs())) {
    if (llvmClass(rhsType) || isListType(rhsType)) {
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
      llvm::Value* var = nullptr;
      bool globalVarDeclFound = false;
      auto currentFn = currentFunction();
      auto found = globalDeclns.find(currentFunction());
      if (found != globalDeclns.end()) {
        if (found->second == idExpr->getId().str()) {
          var = globalVariables[idExpr->getId().str()];
          globalVarDeclFound = true;
        }
      }
      if (!globalVarDeclFound) {
        bool varFoundInClosure = false;
        if (currentFn && currentFn->isNestedFunc()) {
          // check for nonlocal declarations
          const VarInfor* varInfo = scopeManager->lookupVar(idExpr->getId());
          if (varInfo) {
            const auto& [localVar, varDeclaredFn, type, isIterVar] = *varInfo;
            if (varDeclaredFn != currentFn) {
              // lookup the var using ref_env arg
              const VarInfor* refEnvVarInfo =
                  scopeManager->lookupVar("ref_env");
              if (refEnvVarInfo && refEnvVarInfo->funcPtr == currentFn) {
                auto [staticChainDerefCount, varGEPDistance] =
                    calculateClosureDimensions(currentFn,
                                               idExpr->getId().str());
                // load the ref_env address passed to the function
                const FunctionAST* closureFunc = currentFn;
                llvm::Value* refEnvPtr = builder->CreateLoad(
                    nestedFuncToRefEnvType[closureFunc]->getPointerTo(),
                    refEnvVarInfo->var);
                // walk up the static chain
                while (staticChainDerefCount-- > 0) {
                  auto currentRefEnvType = nestedFuncToRefEnvType[closureFunc];
                  // GEP to static parent pointer (assumed index 0)
                  llvm::Value* staticChainGEP = builder->CreateStructGEP(
                      currentRefEnvType, refEnvPtr, 0, "static_chain_gep");
                  // load the parent environment pointer
                  refEnvPtr = builder->CreateLoad(
                      currentRefEnvType->getElementType(0)->getPointerTo(),
                      staticChainGEP);
                  closureFunc = closureFunc->getParentFunc();
                }
                // now access the correct slot in the final environment
                auto finalRefEnvType = nestedFuncToRefEnvType[closureFunc];
                llvm::Value* varGEP =
                    builder->CreateStructGEP(finalRefEnvType, refEnvPtr,
                                             varGEPDistance, "ref_env_var_gep");
                // load the actual variable's LHS = pointer
                llvm::Value* loadedValue = builder->CreateLoad(
                    finalRefEnvType->getElementType(varGEPDistance), varGEP);
                var = loadedValue;
                varFoundInClosure = true;
              }
            }
          }
        }
        if (!varFoundInClosure) {
          var = lookupVariable(idExpr->getId());
        }
      }
      builder->CreateStore(rhsValue, var);
    } else if (auto binaryExpr =
                   llvm::dyn_cast<BinaryExprAST>(varTarget.get())) {
      if (binaryExpr->getOp() == TokenKind::kAttrAccessOp) {
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
      } else if (binaryExpr->getOp() == TokenKind::kIndexAccessOp) {
        // List index access
        binaryExpr->accept(*this);
        llvm::Value* arrPtr = binaryExpr->getCodegenValue();
        builder->CreateStore(rhsValue, arrPtr);
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

void LLVMCodeGenVisitor::visitStmtFor(const StmtForAST& stmtFor) {
  llvm::Function* currentFunc = builder->GetInsertBlock()->getParent();

  // Evaluate the iterable expression
  const ExprAST* iterableExpr = stmtFor.getExpr();
  iterableExpr->accept(*this);
  llvm::Value* iterableVal = iterableExpr->getCodegenValue();
  std::string iterableType = iterableExpr->getTypeInfo();

  // Allocate index variable (int i = 0)
  llvm::AllocaInst* indexAlloca = builder->CreateAlloca(
      llvm::Type::getInt32Ty(*context), nullptr, "for_index");
  builder->CreateStore(
      llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context), 0), indexAlloca);

  // Allocate length variable
  llvm::AllocaInst* lengthAlloca = builder->CreateAlloca(
      llvm::Type::getInt32Ty(*context), nullptr, "for_length");

  // Compute length
  llvm::Value* lengthVal = nullptr;
  if (iterableType == "str") {
    llvm::Function* strLenFunc = module->getFunction("strlength");
    lengthVal = builder->CreateCall(strLenFunc, {iterableVal}, "str_len");
  } else {
    // List type: get length from struct
    llvm::StructType* listStructTy =
        llvm::StructType::get(*context,
                              {llvm::Type::getInt32Ty(*context),
                               llvmTypeOrClassPtrType(iterableType)},
                              false);
    llvm::Value* listStructPtr = iterableVal;
    if (!llvm::isa<ListLiteralExprAST>(iterableExpr)) {
      listStructPtr = builder->CreateLoad(listStructTy->getPointerTo(),
                                          iterableVal, "list_ptr_val");
    }

    llvm::Value* lengthPtr =
        builder->CreateStructGEP(listStructTy, listStructPtr, 0, "length_ptr");
    lengthVal = builder->CreateLoad(llvm::Type::getInt32Ty(*context), lengthPtr,
                                    "length_val");
  }
  builder->CreateStore(lengthVal, lengthAlloca);

  // Allocate loop variable
  TypedVarAST* loopVar = stmtFor.getTypedVar();
  std::string loopVarType = loopVar->getTypeInfo();
  llvm::Type* llvmLoopVarType = llvmTypeOrClassPtrType(loopVarType);
  llvm::AllocaInst* loopVarAlloca =
      builder->CreateAlloca(llvmLoopVarType, nullptr, loopVar->getId());
  scopeManager->addVar(loopVar->getId(), loopVarAlloca, currentFunction(),
                       loopVarType, true);

  // Create basic blocks
  llvm::BasicBlock* forCond =
      llvm::BasicBlock::Create(*context, "for_cond", currentFunc);
  llvm::BasicBlock* forBody =
      llvm::BasicBlock::Create(*context, "for_body", currentFunc);
  llvm::BasicBlock* forMerge =
      llvm::BasicBlock::Create(*context, "for_merge", currentFunc);

  // Branch to condition block
  builder->CreateBr(forCond);

  // Emit condition block
  builder->SetInsertPoint(forCond);
  llvm::Value* idxVal = builder->CreateLoad(llvm::Type::getInt32Ty(*context),
                                            indexAlloca, "idx_val");
  llvm::Value* lenVal = builder->CreateLoad(llvm::Type::getInt32Ty(*context),
                                            lengthAlloca, "len_val");
  llvm::Value* condVal = builder->CreateICmpSLT(idxVal, lenVal, "for_cond");
  builder->CreateCondBr(condVal, forBody, forMerge);

  // Emit body block
  builder->SetInsertPoint(forBody);
  excludeBlockStack.push_back(forMerge);
  // Get element at idxVal
  llvm::Value* elemVal = nullptr;
  if (iterableType == "str") {
    llvm::Function* strIdxFunc = module->getFunction("stridx");
    elemVal =
        builder->CreateCall(strIdxFunc, {iterableVal, idxVal}, "stridx_call");
  } else {
    // List index access
    llvm::StructType* listStructTy =
        llvm::StructType::get(*context,
                              {llvm::Type::getInt32Ty(*context),
                               llvmTypeOrClassPtrType(iterableType)},
                              false);
    llvm::Value* listStructPtr = iterableVal;
    if (!llvm::isa<ListLiteralExprAST>(iterableExpr)) {
      listStructPtr = builder->CreateLoad(listStructTy->getPointerTo(),
                                          iterableVal, "list_ptr_val");
    }
    llvm::Value* arrayPtrPtr =
        builder->CreateStructGEP(listStructTy, listStructPtr, 1, "array_ptr");
    llvm::Value* arrayPtr = builder->CreateLoad(
        listStructTy->getStructElementType(1), arrayPtrPtr, "array_val");
    llvm::Value* elemPtr = builder->CreateGEP(
        llvmTypeOrClassPtrType(loopVarType), arrayPtr, idxVal, "elem_ptr");
    if (isListType(loopVarType)) {
      elemVal = elemPtr;
    } else {
      elemVal = builder->CreateLoad(llvmTypeOrClassPtrType(loopVarType),
                                    elemPtr, "elem_val");
    }
  }
  // Store element in loop variable
  builder->CreateStore(elemVal, loopVarAlloca);

  // Emit body statements
  for (const auto& stmt : stmtFor.getBody()) {
    stmt->accept(*this);
  }

  // Increment index
  llvm::Value* nextIdx = builder->CreateAdd(
      idxVal, llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context), 1),
      "next_idx");
  builder->CreateStore(nextIdx, indexAlloca);

  // Get any unterminated blocks in the body
  auto unterminated = getUnterminatedBlocks(currentFunc);
  for (auto* block : unterminated) {
    builder->SetInsertPoint(block);
    if (!block->getTerminator()) {
      builder->CreateBr(forCond);
    }
  }
  excludeBlockStack.pop_back();

  // Set insert point to merge block for code after the loop
  builder->SetInsertPoint(forMerge);
}

void LLVMCodeGenVisitor::visitSimpleStmtReturn(
    const SimpleStmtReturnAST& simpleStmtReturn) {
  if (simpleStmtReturn.getExpr()) {
    simpleStmtReturn.getExpr()->accept(*this);
    llvm::Value* retValue = simpleStmtReturn.getExpr()->getCodegenValue();
    std::string retValueType = simpleStmtReturn.getExpr()->getTypeInfo();
    if (auto rhsId = llvm::dyn_cast<IdExprAST>(simpleStmtReturn.getExpr())) {
      if (llvmClass(retValueType) || isListType(retValueType)) {
        llvm::Type* llvmType = llvmTypeOrClassPtrType(retValueType);
        retValue = builder->CreateLoad(llvmType, retValue, "loaded_ret_value");
      }
    }
    builder->CreateRet(retValue);
  } else {
    builder->CreateRet(
        llvmDefaultValue(currentFunction()->getReturnType()->getTypeName()));
  }
}

llvm::Value* LLVMCodeGenVisitor::lookupVariable(llvm::StringRef varName) {
  const VarInfor* varInfo = scopeManager->lookupVar(varName);
  if (varInfo)
    return (*varInfo).var;
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
  if (typeName == "<Empty>") {
    return llvmClass("object")->getPointerTo();
  }
  llvm::Type* type = llvmType(typeName);
  if (type == nullptr) {
    size_t pos = typeName.find('[');
    if (pos != std::string::npos) {
      type = llvmTypeOrClassPtrType(typeName.substr(0, pos))->getPointerTo();
    } else {
      type = llvmClass(typeName)->getPointerTo();
    }
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
  if (function->isNestedFunc()) {
    createNestedFuncDecl(function);
    return nestedFuncs.at(function);
  }
  return functions.at(function);
}

std::string LLVMCodeGenVisitor::getFQN(const FunctionAST* func) {
  std::string fullyQualifiedName = func->getId().str();
  const FunctionAST* parentFunction = currentFunction();
  while (parentFunction) {
    if (fullyQualifiedName != parentFunction->getId().str()) {
      fullyQualifiedName =
          parentFunction->getId().str() + "-" + fullyQualifiedName;
    }
    parentFunction = parentFunction->getParentFunc();
  }
  if (currentClass) {
    fullyQualifiedName = currentClass->getId().str() + "-" + fullyQualifiedName;
  }
  return fullyQualifiedName;
}

void LLVMCodeGenVisitor::createReferenceEnvType(const FunctionAST* nestedFunc) {
  // Get parent function create a reference env type using all of it's args and
  // local variables
  const FunctionAST* parentFunction = nestedFunc->getParentFunc();
  llvm::StructType* refEnvStructType =
      llvm::StructType::create(*context, getFQN(nestedFunc));

  std::vector<llvm::Type*> fieldTypes = {llvm::Type::getInt8PtrTy(*context)};

  for (auto& arg : parentFunction->getArgs()) {
    fieldTypes.push_back(llvm::PointerType::getUnqual(
        llvmTypeOrClassPtrType(arg->getTypeInfo())));
  }

  for (auto& varDef : parentFunction->getVarDefs()) {
    fieldTypes.push_back(llvm::PointerType::getUnqual(llvmTypeOrClassPtrType(
        varDef->getTypedVar()->getType()->getTypeName())));
  }

  refEnvStructType->setBody(fieldTypes);
  nestedFuncToRefEnvType[nestedFunc] = refEnvStructType;
}

void LLVMCodeGenVisitor::createNestedFuncDecl(const FunctionAST* nestedFunc) {
  if (nestedFunc->isNestedFunc()) {
    llvm::Type* retType =
        llvmTypeOrClassPtrType(nestedFunc->getReturnType()->getTypeName());

    std::vector<llvm::Type*> argTypes;
    for (const auto& arg : nestedFunc->getArgs()) {
      argTypes.push_back(llvmTypeOrClassPtrType(arg->getType()->getTypeName()));
    }

    std::string funcName = nestedFunc->getId().str();
    const FunctionAST* parentPtr = nestedFunc->getParentFunc();
    while (parentPtr) {
      funcName = parentPtr->getId().str() + "-" + funcName;
      parentPtr = parentPtr->getParentFunc();
    }

    llvm::FunctionType* funcType =
        llvm::FunctionType::get(retType, argTypes, false);
    llvm::Function* func = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, funcName, *module);

    nestedFuncs[nestedFunc] = func;
  }
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

/***********************************/
/* ScopeManager                    */
/***********************************/

void ScopeManager::pushScope() { localVarStack.push_back({}); }

void ScopeManager::popScope() { localVarStack.pop_back(); }

void ScopeManager::addVar(llvm::StringRef name, llvm::Value* alloca,
                          const FunctionAST* fnPtr, const std::string& type,
                          bool isIterVar) {
  localVarStack.back()[name.str()] = {alloca, fnPtr, type, isIterVar};
}

const VarInfor* ScopeManager::lookupVar(llvm::StringRef name) const {
  for (auto scopeIt = localVarStack.rbegin(); scopeIt != localVarStack.rend();
       ++scopeIt) {
    auto found = scopeIt->find(name.str());
    if (found != scopeIt->end()) {
      return &found->second;
    }
  }
  return nullptr;
}

} // namespace chocopy
