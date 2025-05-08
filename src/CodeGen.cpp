#include <filesystem>
#include <iostream>

#include "CodeGen.h"

namespace chocopy {

LLVMCodeGenVisitor::LLVMCodeGenVisitor()
    : context(std::make_unique<llvm::LLVMContext>()),
      builder(std::make_unique<llvm::IRBuilder<>>(*context)) {}

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

void LLVMCodeGenVisitor::codeGen(const chocopy::ProgramAST& program,
                                 llvm::StringRef programPath) {
  std::filesystem::path path(programPath.data());
  std::string fileName = path.filename().string();

  module = std::make_unique<llvm::Module>(fileName, *context);

  /* Include external function declarations */
  createBuiltinFuncDecl("printf", "int", {"str"}, true);

  program.accept(*this);
}

void LLVMCodeGenVisitor::visitProgram(const ProgramAST& program) {
  codeGenMainFunc(program.getStmts());
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

void LLVMCodeGenVisitor::visitVarDef(const VarDefAST& varDef) {}

void LLVMCodeGenVisitor::visitTypedVar(const TypedVarAST& typedVar) {}

void LLVMCodeGenVisitor::createBuiltinFuncDecl(
    const std::string& funcName, const std::string& returnType,
    const std::vector<std::string>& argTypes, bool isVarArg) const {
  std::vector<llvm::Type*> llvmArgTypes;
  for (const auto& arg : argTypes) {
    llvmArgTypes.push_back(llvmType(arg));
  }

  llvm::Type* llvmReturnType;
  if (returnType == "<None>") {
    llvmReturnType = builder->getVoidTy();
  } else {
    llvmReturnType = llvmType(returnType);
  }

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
  if (typeName == "int")
    return llvm::Type::getInt32Ty(*context);
  if (typeName == "str")
    return llvm::Type::getInt8PtrTy(*context);
  if (typeName == "bool")
    return llvm::Type::getInt1Ty(*context);
  return nullptr;
}

} // namespace chocopy
