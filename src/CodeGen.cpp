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

  createClassTypesAndVtableTypes(programAST->getClassDefs());

  /* Include external function declarations */
  createBuiltinFuncDecl("printf", "int", {"str"});
  createBuiltinFuncDecl("malloc", "str", {"int"});

  programAST->accept(*this);
}

void LLVMCodeGenVisitor::visitProgram(const ProgramAST& program) {
  for (auto& globalVarDef : program.getVarDefs()) {
    globalVarDef->accept(*this);
  }

  for (auto& clazz : program.getClassDefs()) {
    currentClass = clazz.get();
    // add attributes and methods
    addAttributes(clazz.get());
    addMethods(clazz.get());

    // TODO: remove temp
    llvm::GlobalVariable* dummy = new llvm::GlobalVariable(
        *module, llvmClass(clazz.get()), false,
        llvm::GlobalValue::InternalLinkage, nullptr, "dummy_" + clazz->getId());
  }

  codeGenMainFunc(program.getStmts());
}

void LLVMCodeGenVisitor::addAttributes(const ClassAST* classPtr) {
  std::vector<llvm::Type*> attributeTypes;
  // Add vtable ptr as the first thing in any class type
  attributeTypes.push_back(
      getVTable(classPtr).GetVTStructType()->getPointerTo());

  // TODO: Add inherited and class attributes

  classToStructType.at(classPtr)->setBody(attributeTypes);
}

void LLVMCodeGenVisitor::addMethods(const ClassAST* classPtr) {
  std::vector<llvm::Constant*> vtableFuns;

  if (classPtr->getSuperClassId().str() != "object") {
    const std::vector<llvm::Constant*>& superClassVTableFuncs =
        getVTable(classPtr->getSuperClassId().str()).getFuncs();

    vtableFuns.insert(vtableFuns.end(), superClassVTableFuncs.begin(),
                      superClassVTableFuncs.end());
  }

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

    functions[methodDef.get()] = func;
    // TODO: Add new methods and overriden methods for this class
  }
}

void LLVMCodeGenVisitor::visitClass(const ClassAST& clazz) {}

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

void LLVMCodeGenVisitor::visitCallExpr(const CallExprAST& callExpr) {
  for (auto& arg : callExpr.getArgs()) {
    arg->accept(*this);
  }
  llvm::Function* calleeFunc = nullptr;
  if (auto callee = llvm::dyn_cast<IdExprAST>(callExpr.getCallee())) {
    if (callee->getId() == "print") {
      calleeFunc = module->getFunction("printf");
      if (!calleeFunc) {
        llvm::errs() << "Unknown function referenced 'printf'\n";
        return;
      }
    }
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
    } else {
      // class method var def (stack var)
    }
  } else {
    if (currentFunction == nullptr) {
      // global var def

    } else {
      // global function var def
    }
  }
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

llvm::Type*
LLVMCodeGenVisitor::llvmTypeOrClassPtrType(const std::string& typeName) {
  llvm::Type* type = llvmType(typeName);
  if (type == nullptr) {
    type = llvmClass(typeName)->getPointerTo();
  }
  return type;
}

llvm::StructType* LLVMCodeGenVisitor::llvmClass(const std::string& className) {
  return llvmClass(getClassByName(className));
}

llvm::StructType* LLVMCodeGenVisitor::llvmClass(const ClassAST* classPtr) {
  return classToStructType.at(classPtr);
}

const ClassAST* LLVMCodeGenVisitor::getClassByName(std::string name) const {
  if (name == "self") {
    name = currentClass->getId();
  }
  return programAST->GetClassPtr(name);
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

} // namespace chocopy
