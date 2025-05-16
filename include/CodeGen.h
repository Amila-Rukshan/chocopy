#ifndef CHOCOPY_CODEGEN_H
#define CHOCOPY_CODEGEN_H

#include <array>
#include <map>

#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"

#include "AST.h"

namespace chocopy {

class VirtualTable;

class LLVMCodeGenVisitor : public ASTVisitor {
public:
  LLVMCodeGenVisitor(ProgramAST* program, llvm::StringRef programPath);
  ~LLVMCodeGenVisitor();

  void codeGen();
  void codeGenMainFunc(const std::vector<std::unique_ptr<StmtAST>>& stmts);
  void printLLVMBitCode(llvm::StringRef outputPath) const;

  void visitProgram(const ProgramAST& program) override;
  void visitClass(const ClassAST& clazz) override;
  void visitFunction(const FunctionAST& func) override;
  void visitLiteralNumber(const LiteralNumberAST& literalNumber) override;
  void visitLiteralTrue(const LiteralTrueAST& literalTrue) override;
  void visitLiteralFalse(const LiteralFalseAST& literalFalse) override;
  void visitLiteralString(const LiteralStringAST& literalString) override;
  void visitLiteralNone(const LiteralNoneAST& literalNone) override;
  void visitCallExpr(const CallExprAST& callExpr) override;
  void visitIdExpr(const IdExprAST& idExpr) override;
  void visitBinaryExpr(const BinaryExprAST& binaryExpr) override;
  void visitVarDef(const VarDefAST& varDef) override;
  void visitTypedVar(const TypedVarAST& typedVar) override;

  void
  visitSimpleStmtAssign(const SimpleStmtAssignAST& simpleStmtAssign) override;
  void visitSimpleStmtExpr(const SimpleStmtExprAST& simpleStmtExpr) override;
  void
  visitSimpleStmtReturn(const SimpleStmtReturnAST& simpleStmtReturn) override;

private:
  void createBuiltinFuncDecl(const std::string& funcName,
                             const std::string& returnType,
                             const std::vector<std::string>& argTypes,
                             bool isVarArg = false) const;

  void createBuiltinFuncDecl(const std::string& funcName,
                             llvm::Type* returnType,
                             const std::vector<llvm::Type*>& llvmArgTypes,
                             bool isVarArg = false) const;

  void createClassTypesAndVtableTypes(
      const std::vector<std::unique_ptr<ClassAST>>& classDefs);
  void addClassAttributes();
  const VirtualTable& getVTable(const ClassAST* classPtr);
  const VirtualTable& getVTable(const std::string& className);

  llvm::Type* llvmType(std::string typeName) const;
  llvm::StructType* llvmClass(const std::string& className);
  llvm::StructType* llvmClass(const ClassAST* classPtr);
  const ClassAST* getClassByName(std::string name) const;
  llvm::Type* llvmTypeOrClassPtrType(const std::string& typeName);
  void addAttributes(const ClassAST* classPtr);
  void addMethods(const ClassAST* classPtr);
  std::string getRetTypeName(const TypeAST* type);
  llvm::Constant* llvmDefaultValue(const std::string& typeName);
  llvm::Function* llvmFunc(const FunctionAST* function);
  llvm::Value* lookupVariable(llvm::StringRef varName);

  std::unique_ptr<llvm::LLVMContext> context;
  std::unique_ptr<llvm::IRBuilder<>> builder;
  std::unique_ptr<llvm::Module> module;

  ProgramAST* programAST = nullptr;
  ClassAST* currentClass = nullptr;
  FunctionAST* currentFunction = nullptr;
  llvm::StringRef programPath;

  std::unordered_map<const ClassAST*, llvm::StructType*> classToStructType;
  std::unordered_map<const ClassAST*, VirtualTable> classToVTable;
  std::unordered_map<const FunctionAST*, llvm::Function*> functions;
  std::unordered_map<std::string, llvm::Function*> functionNameToFunc;
  std::map<llvm::StringRef, llvm::GlobalVariable*> globalVariables;
  std::map<llvm::StringRef, llvm::AllocaInst*> localVariables;

  std::unordered_map<const ClassAST*,
                     std::unordered_map<std::string, std::vector<uint32_t>>>
      classFieldGEPMap;
};

class VirtualTable {
public:
  VirtualTable(llvm::LLVMContext& context, const ClassAST* classAST)
      : classAST(classAST), virtualTableStructType(llvm::StructType::create(
                                context, classAST->getId().str() + "-vtbl")) {}
  llvm::StructType* GetVTStructType() const { return virtualTableStructType; }
  void createVTable(llvm::Module* module,
                    const std::vector<llvm::Constant*>& vtableFuncs);
  const std::vector<llvm::Constant*>& getFuncs() const;
  llvm::GlobalValue* getGlobalVTableVal() const;
  size_t getVTableIndex(llvm::Constant* llvmFunc) const;

private:
  const ClassAST* classAST;
  llvm::StructType* virtualTableStructType;
  llvm::GlobalValue* globalVTableVal = nullptr;
  std::vector<llvm::Constant*> funcs;
};

} // namespace chocopy

#endif // CHOCOPY_CODEGEN_H
