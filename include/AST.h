#ifndef CHOCOPY_AST_H
#define CHOCOPY_AST_H

#include "Token.h"

#include <llvm/ADT/StringRef.h>
#include <llvm/IR/Value.h>

#include <memory>
#include <string>
#include <vector>

namespace chocopy {

class VarDefAST;
class ExprAST;
class StmtAST;
class TypeAST;
class TypedVarAST;
class FunctionAST;
class ClassAST;
class ProgramAST;

class LiteralNumberAST;
class LiteralTrueAST;
class LiteralFalseAST;
class LiteralStringAST;
class LiteralNoneAST;

class CallExprAST;

class ASTVisitor {
public:
  virtual ~ASTVisitor() = default;

  virtual void visitProgram(const ProgramAST& program) = 0;

  virtual void visitClass(const ClassAST& clazz) = 0;
  virtual void visitFunction(const FunctionAST& func) = 0;

  virtual void visitLiteralNumber(const LiteralNumberAST& literalNumber) = 0;
  virtual void visitLiteralTrue(const LiteralTrueAST& literalTrue) = 0;
  virtual void visitLiteralFalse(const LiteralFalseAST& literalFalse) = 0;
  virtual void visitLiteralString(const LiteralStringAST& literalString) = 0;
  virtual void visitLiteralNone(const LiteralNoneAST& literalNone) = 0;
  virtual void visitCallExpr(const CallExprAST& callExpr) = 0;
  virtual void visitVarDef(const VarDefAST& varDef) = 0;
  virtual void visitTypedVar(const TypedVarAST& typedVar) = 0;
};

/***********************************/
/* Class                           */
/***********************************/

class ClassAST {
public:
  ClassAST(Location location, std::string id, Location superClassLocation,
           std::string superClassId,
           std::vector<std::unique_ptr<VarDefAST>> varDefs,
           std::vector<std::unique_ptr<FunctionAST>> funcDefs)
      : location(location), id(std::move(id)),
        superClassLocation(std::move(superClassLocation)),
        superClassId(std::move(superClassId)), varDefs(std::move(varDefs)),
        funcDefs(std::move(funcDefs)) {}

  void accept(ASTVisitor& visitor) { visitor.visitClass(*this); };

  const llvm::StringRef getId() const { return id; }
  const llvm::StringRef getSuperClassId() const { return superClassId; }
  const std::vector<std::unique_ptr<FunctionAST>>& getMethodDefs() const {
    return funcDefs;
  }
  const std::vector<std::unique_ptr<VarDefAST>>& getVarDefs() const {
    return varDefs;
  }

  const Location& classLoc() { return location; }
  const Location& superClassLoc() { return superClassLocation; }

  void setParentClass(const ClassAST* classPtr) { parentClass = classPtr; }

  void AddChildClass(const ClassAST* classPtr) {
    childClasses.push_back(classPtr);
  }

private:
  Location location;
  const std::string id;
  Location superClassLocation;
  const std::string superClassId;
  std::vector<std::unique_ptr<VarDefAST>> varDefs;
  std::vector<std::unique_ptr<FunctionAST>> funcDefs;
  std::vector<const ClassAST*> childClasses;
  const ClassAST* parentClass;
};

/***********************************/
/* Program                         */
/***********************************/

class ProgramAST {
public:
  ProgramAST(std::vector<std::unique_ptr<VarDefAST>> varDefs,
             std::vector<std::unique_ptr<FunctionAST>> funcDefs,
             std::vector<std::unique_ptr<ClassAST>> classDefs,
             std::vector<std::unique_ptr<StmtAST>> stmts)
      : varDefs(std::move(varDefs)), funcDefs(std::move(funcDefs)),
        classDefs(std::move(classDefs)), stmts(std::move(stmts)),
        objectClass(std::make_unique<ClassAST>(
            Location{nullptr, 0, 0}, "object", Location{nullptr, 0, 0}, "epic",
            std::vector<std::unique_ptr<VarDefAST>>(),
            std::vector<std::unique_ptr<FunctionAST>>())) {
    for (auto& classDef : this->classDefs) {
      classIdToClassAST[classDef->getId().str()] = classDef.get();
    }
    classIdToClassAST["object"] = objectClass.get();
  }

  const std::vector<std::unique_ptr<VarDefAST>>& getVarDefs() const {
    return varDefs;
  }
  const std::vector<std::unique_ptr<FunctionAST>>& getFuncDefs() const {
    return funcDefs;
  }
  const std::vector<std::unique_ptr<ClassAST>>& getClassDefs() const {
    return classDefs;
  }
  const std::vector<std::unique_ptr<StmtAST>>& getStmts() const {
    return stmts;
  }

  void accept(ASTVisitor& visitor) const { visitor.visitProgram(*this); }

  ClassAST* GetClassPtr(const std::string& classId) const {
    auto classAST = classIdToClassAST.find(classId);
    if (classAST == classIdToClassAST.end()) {
      return nullptr;
    } else {
      return classAST->second;
    }
  }

private:
  std::vector<std::unique_ptr<VarDefAST>> varDefs;
  std::vector<std::unique_ptr<FunctionAST>> funcDefs;
  std::vector<std::unique_ptr<ClassAST>> classDefs;
  std::vector<std::unique_ptr<StmtAST>> stmts;

  std::unique_ptr<ClassAST> objectClass;
  std::unordered_map<std::string, ClassAST*> classIdToClassAST;
};

/***********************************/
/* Function                        */
/***********************************/

class FunctionAST {
public:
  FunctionAST(Location location, std::string id,
              std::vector<std::unique_ptr<TypedVarAST>> args,
              std::unique_ptr<TypeAST> returnType,
              std::vector<std::unique_ptr<VarDefAST>> varDefs,
              std::vector<std::string> globalDecls,
              std::vector<std::string> nonlocalDecls,
              std::vector<std::unique_ptr<FunctionAST>> funcDefs,
              std::vector<std::unique_ptr<StmtAST>> body)
      : location(location), id(std::move(id)), args(std::move(args)),
        returnType(std::move(returnType)), varDefs(std::move(varDefs)),
        globalDecls(std::move(globalDecls)),
        nonlocalDecls(std::move(nonlocalDecls)), funcDefs(std::move(funcDefs)),
        body(std::move(body)) {}

  const llvm::StringRef getId() const { return id; }
  const std::vector<std::unique_ptr<TypedVarAST>>& getArgs() const {
    return args;
  }
  const TypeAST* getReturnType() const { return returnType.get(); }
  const std::vector<std::unique_ptr<VarDefAST>>& getVarDefs() const {
    return varDefs;
  }
  const std::vector<std::string>& getGlobalDecls() const { return globalDecls; }
  const std::vector<std::string>& getNonlocalDecls() const {
    return nonlocalDecls;
  }
  const std::vector<std::unique_ptr<FunctionAST>>& getFuncDefs() const {
    return funcDefs;
  }
  const std::vector<std::unique_ptr<StmtAST>>& getBody() const { return body; }

  void accept(ASTVisitor& visitor) const { visitor.visitFunction(*this); };

private:
  Location location;
  const std::string id;
  std::vector<std::unique_ptr<TypedVarAST>> args;
  std::unique_ptr<TypeAST> returnType;
  std::vector<std::unique_ptr<VarDefAST>> varDefs;
  std::vector<std::string> globalDecls;
  std::vector<std::string> nonlocalDecls;
  std::vector<std::unique_ptr<FunctionAST>> funcDefs;
  std::vector<std::unique_ptr<StmtAST>> body;
};

/***********************************/
/* Type                            */
/***********************************/

class TypeAST {
public:
  enum TypeASTKind { Type_Id, Type_IdString, Type_List };
  TypeAST(TypeASTKind kind, Location location)
      : kind(kind), location(std::move(location)) {}

  virtual ~TypeAST() = default;

  TypeASTKind getKind() const { return kind; }

  const Location& loc() const { return location; }

  virtual std::string getTypeName() const = 0;

private:
  const TypeASTKind kind;
  Location location;
};

class IdTypeAST : public TypeAST {
public:
  IdTypeAST(Location location, std::string id)
      : TypeAST(TypeAST::Type_Id, std::move(location)), id(std::move(id)) {}

  const llvm::StringRef getId() const { return id; }

  std::string getTypeName() const override { return id; }

  /// LLVM style RTTI
  static bool classof(const TypeAST* c) {
    return c->getKind() == TypeAST::Type_Id;
  }

private:
  const std::string id;
};

class IdStringTypeAST : public TypeAST {
public:
  IdStringTypeAST(Location location, std::string id)
      : TypeAST(TypeAST::Type_IdString, std::move(location)),
        id(std::move(id)) {}

  const llvm::StringRef getId() const { return id; }

  /// LLVM style RTTI
  static bool classof(const TypeAST* c) {
    return c->getKind() == TypeAST::Type_IdString;
  }

  std::string getTypeName() const override { return id; }

private:
  const std::string id;
};

class ListTypeAST : public TypeAST {
public:
  ListTypeAST(Location location, std::unique_ptr<TypeAST> type, int dimension)
      : TypeAST(TypeAST::Type_List, std::move(location)), type(std::move(type)),
        dimension(dimension), computedTypeName(this->type->getTypeName() +
                                               std::string(dimension, '[') +
                                               std::string(dimension, ']')) {}

  const TypeAST* getType() const { return type.get(); }
  int getDimension() const { return dimension; }

  /// LLVM style RTTI
  static bool classof(const TypeAST* c) {
    return c->getKind() == TypeAST::Type_List;
  }

  std::string getTypeName() const override { return computedTypeName; }

private:
  std::unique_ptr<TypeAST> type;
  int dimension = 0;
  std::string computedTypeName;
};

/***********************************/
/* TypedVar                        */
/***********************************/

class TypedVarAST {
public:
  TypedVarAST(std::string id, Location location, std::unique_ptr<TypeAST> type)
      : id(std::move(id)), type(std::move(type)),
        location(std::move(location)) {}

  const llvm::StringRef getId() const { return id; }

  const TypeAST* getType() const { return type.get(); }
  const Location& loc() const { return location; }

  void accept(ASTVisitor& visitor) const { visitor.visitTypedVar(*this); };

  void setTypeInfo(const std::string& type) const { typeInfo->type = type; }

  const std::string& getTypeInfo() const { return typeInfo->type; }

private:
  struct TypeInfo {
    std::string type = "UNTYPED";
  };
  std::unique_ptr<TypeInfo> typeInfo = std::make_unique<TypeInfo>();
  const std::string id;
  std::unique_ptr<TypeAST> type;
  Location location;
};

/***********************************/
/* Literal                         */
/***********************************/

class LiteralAST {
public:
  enum LiteralASTKind {
    Literal_None,
    Literal_True,
    Literal_False,
    Literal_Number,
    Literal_String,
    Literal_IdString
  };

  LiteralAST(LiteralASTKind kind, Location location)
      : kind(kind), location(std::move(location)) {}
  virtual ~LiteralAST() = default;

  LiteralASTKind getKind() const { return kind; }

  const Location& loc() const { return location; }

  virtual void accept(ASTVisitor& visitor) const = 0;

  void setTypeInfo(const std::string& type) const { typeInfo->type = type; }

  const std::string& getTypeInfo() const { return typeInfo->type; }

  void setCodegenValue(llvm::Value* value) const { codegenInfo->value = value; }

  llvm::Value* getCodegenValue() const { return codegenInfo->value; }

private:
  struct TypeInfo {
    std::string type = "UNTYPED";
  };
  std::unique_ptr<TypeInfo> typeInfo = std::make_unique<TypeInfo>();
  struct CodegenInfo {
    llvm::Value* value = nullptr;
  };
  std::unique_ptr<CodegenInfo> codegenInfo = std::make_unique<CodegenInfo>();
  const LiteralASTKind kind;
  Location location;
};

class LiteralNoneAST : public LiteralAST {
public:
  LiteralNoneAST(Location location)
      : LiteralAST(LiteralAST::Literal_None, std::move(location)) {}

  void accept(ASTVisitor& visitor) const override {
    visitor.visitLiteralNone(*this);
  }

  /// LLVM style RTTI
  static bool classof(const LiteralAST* c) {
    return c->getKind() == LiteralAST::Literal_None;
  }
};

class LiteralTrueAST : public LiteralAST {
public:
  LiteralTrueAST(Location location)
      : LiteralAST(LiteralAST::Literal_True, std::move(location)) {}

  void accept(ASTVisitor& visitor) const override {
    visitor.visitLiteralTrue(*this);
  }

  /// LLVM style RTTI
  static bool classof(const LiteralAST* c) {
    return c->getKind() == LiteralAST::Literal_True;
  }
};

class LiteralFalseAST : public LiteralAST {
public:
  LiteralFalseAST(Location location)
      : LiteralAST(LiteralAST::Literal_False, std::move(location)) {}

  void accept(ASTVisitor& visitor) const override {
    visitor.visitLiteralFalse(*this);
  }

  /// LLVM style RTTI
  static bool classof(const LiteralAST* c) {
    return c->getKind() == LiteralAST::Literal_False;
  }
};

class LiteralNumberAST : public LiteralAST {
public:
  LiteralNumberAST(Location location, int number)
      : LiteralAST(LiteralAST::Literal_Number, std::move(location)),
        number(number) {}

  int getNumber() const { return number; }

  void accept(ASTVisitor& visitor) const override {
    visitor.visitLiteralNumber(*this);
  }

  /// LLVM style RTTI
  static bool classof(const LiteralAST* c) {
    return c->getKind() == LiteralAST::Literal_Number;
  }

private:
  const int number;
};

class LiteralStringAST : public LiteralAST {
public:
  LiteralStringAST(Location location, std::string str)
      : LiteralAST(LiteralAST::Literal_String, std::move(location)),
        str(std::move(str)) {}

  const llvm::StringRef getStr() const { return str; }

  void accept(ASTVisitor& visitor) const override {
    visitor.visitLiteralString(*this);
  }

  /// LLVM style RTTI
  static bool classof(const LiteralAST* c) {
    return c->getKind() == LiteralAST::Literal_String;
  }

private:
  const std::string str;
};

class LiteralIdStringAST : public LiteralAST {
public:
  LiteralIdStringAST(Location location, std::string id)
      : LiteralAST(LiteralAST::Literal_IdString, std::move(location)),
        id(std::move(id)) {}

  const llvm::StringRef getId() const { return id; }

  void accept(ASTVisitor& visitor) const override {}

  /// LLVM style RTTI
  static bool classof(const LiteralAST* c) {
    return c->getKind() == LiteralAST::Literal_IdString;
  }

private:
  const std::string id;
};

/***********************************/
/* VarDef                          */
/***********************************/

class VarDefAST {
public:
  VarDefAST(std::unique_ptr<TypedVarAST> typedVar,
            std::unique_ptr<LiteralAST> literal)
      : typedVar(std::move(typedVar)), literal(std::move(literal)) {}
  const TypedVarAST* getTypedVar() const { return typedVar.get(); }
  const LiteralAST* getLiteral() const { return literal.get(); }
  void accept(ASTVisitor& visitor) const { visitor.visitVarDef(*this); };

private:
  std::unique_ptr<TypedVarAST> typedVar;
  std::unique_ptr<LiteralAST> literal;
};

/***********************************/
/* Expression                      */
/***********************************/

class ExprAST {

public:
  enum ExprASTKind {
    Expr_Id,
    Expr_Literal,
    Expr_ListLiteral,
    Expr_Call,
    Expr_Print,
    Expr_Input,
    Expr_Len,
    Expr_BinaryOp,
    Expr_UnaryOp,
    Expr_IfElse,
  };

  ExprAST(ExprASTKind kind, Location location)
      : kind(kind), location(std::move(location)) {}
  virtual ~ExprAST() = default;

  ExprASTKind getKind() const { return kind; }

  const Location& loc() const { return location; }

  virtual void accept(ASTVisitor& visitor) const = 0;

  void setTypeInfo(const std::string& type) const { typeInfo->type = type; }
  const std::string& getTypeInfo() const { return typeInfo->type; }

  void setCodegenValue(llvm::Value* value) const { codegenInfo->value = value; }
  llvm::Value* getCodegenValue() const { return codegenInfo->value; }

private:
  struct TypeInfo {
    std::string type = "UNTYPED";
  };
  std::unique_ptr<TypeInfo> typeInfo = std::make_unique<TypeInfo>();
  struct CodegenInfo {
    llvm::Value* value = nullptr;
  };
  std::unique_ptr<CodegenInfo> codegenInfo = std::make_unique<CodegenInfo>();
  const ExprASTKind kind;
  Location location;
};

class IdExprAST : public ExprAST {
public:
  IdExprAST(Location location, std::string id)
      : ExprAST(ExprAST::Expr_Id, std::move(location)), id(std::move(id)) {}

  const llvm::StringRef getId() const { return id; }

  void accept(ASTVisitor& visitor) const override {}

  /// LLVM style RTTI
  static bool classof(const ExprAST* c) {
    return c->getKind() == ExprAST::Expr_Id;
  }

private:
  const std::string id;
};

class LiteralExprAST : public ExprAST {
public:
  LiteralExprAST(Location location, std::unique_ptr<LiteralAST> literal)
      : ExprAST(ExprAST::Expr_Literal, std::move(location)),
        literal(std::move(literal)) {}

  const LiteralAST* getLiteral() const { return literal.get(); }

  void accept(ASTVisitor& visitor) const override {
    literal->accept(visitor);
    setCodegenValue(literal->getCodegenValue());
    setTypeInfo(literal->getTypeInfo());
  }

  /// LLVM style RTTI
  static bool classof(const ExprAST* c) {
    return c->getKind() == ExprAST::Expr_Literal;
  }

private:
  std::unique_ptr<LiteralAST> literal;
};

class ListLiteralExprAST : public ExprAST {
public:
  ListLiteralExprAST(Location location,
                     std::vector<std::unique_ptr<ExprAST>> elements)
      : ExprAST(ExprAST::Expr_ListLiteral, std::move(location)),
        elements(std::move(elements)) {}

  const std::vector<std::unique_ptr<ExprAST>>& getElements() const {
    return elements;
  }

  void accept(ASTVisitor& visitor) const override {}

  // LLVM style RTTI
  static bool classof(const ExprAST* c) {
    return c->getKind() == ExprAST::Expr_ListLiteral;
  }

private:
  std::vector<std::unique_ptr<ExprAST>> elements;
};

class CallExprAST : public ExprAST {
public:
  CallExprAST(Location location, std::unique_ptr<ExprAST> callee,
              std::vector<std::unique_ptr<ExprAST>> args)
      : ExprAST(ExprAST::Expr_Call, std::move(location)),
        callee(std::move(callee)), args(std::move(args)) {}

  const ExprAST* getCallee() const { return callee.get(); }
  const std::vector<std::unique_ptr<ExprAST>>& getArgs() const { return args; }

  void accept(ASTVisitor& visitor) const override {
    visitor.visitCallExpr(*this);
  }

  // LLVM style RTTI
  static bool classof(const ExprAST* c) {
    return c->getKind() == ExprAST::Expr_Call;
  }

private:
  std::unique_ptr<ExprAST> callee;
  std::vector<std::unique_ptr<ExprAST>> args;
};

class PrintExprAST : public ExprAST {
public:
  PrintExprAST(Location location, std::unique_ptr<ExprAST> printArg)
      : ExprAST(ExprAST::Expr_Print, std::move(location)),
        printArg(std::move(printArg)) {}

  const std::unique_ptr<ExprAST>& getArgs() const { return printArg; }

  void accept(ASTVisitor& visitor) const override {}

  // LLVM style RTTI
  static bool classof(const ExprAST* c) {
    return c->getKind() == ExprAST::Expr_Print;
  }

private:
  std::unique_ptr<ExprAST> printArg;
};

class InputExprAST : public ExprAST {
public:
  InputExprAST(Location location, std::string id)
      : ExprAST(ExprAST::Expr_Input, std::move(location)) {}

  void accept(ASTVisitor& visitor) const override {}

  // LLVM style RTTI
  static bool classof(const ExprAST* c) {
    return c->getKind() == ExprAST::Expr_Input;
  }
};

class LenExprAST : public ExprAST {
public:
  LenExprAST(Location location, std::unique_ptr<ExprAST> expr)
      : ExprAST(ExprAST::Expr_Len, std::move(location)) {}

  void accept(ASTVisitor& visitor) const override {}

  // LLVM style RTTI
  static bool classof(const ExprAST* c) {
    return c->getKind() == ExprAST::Expr_Len;
  }
};

class IfElseExprAST : public ExprAST {
public:
  IfElseExprAST(Location location, std::unique_ptr<ExprAST> condition,
                std::unique_ptr<ExprAST> ifBody,
                std::unique_ptr<ExprAST> elseBody)
      : ExprAST(ExprAST::Expr_IfElse, std::move(location)),
        condition(std::move(condition)), ifBody(std::move(ifBody)),
        elseBody(std::move(elseBody)) {}

  const ExprAST* getCondition() const { return condition.get(); }
  const ExprAST* getIfBody() const { return ifBody.get(); }
  const ExprAST* getElseBody() const { return elseBody.get(); }

  void accept(ASTVisitor& visitor) const override {}

  // LLVM style RTTI
  static bool classof(const ExprAST* c) {
    return c->getKind() == ExprAST::Expr_IfElse;
  }

private:
  std::unique_ptr<ExprAST> condition;
  std::unique_ptr<ExprAST> ifBody;
  std::unique_ptr<ExprAST> elseBody;
};

class BinaryExprAST : public ExprAST {
public:
  BinaryExprAST(Location location, std::unique_ptr<ExprAST> lhs,
                std::unique_ptr<ExprAST> rhs, TokenKind op)
      : ExprAST(ExprAST::Expr_BinaryOp, std::move(location)),
        lhs(std::move(lhs)), rhs(std::move(rhs)), op(op) {}

  const ExprAST* getLhs() const { return lhs.get(); }
  const ExprAST* getRhs() const { return rhs.get(); }
  TokenKind getOp() const { return op; }

  void accept(ASTVisitor& visitor) const override {}

  // LLVM style RTTI
  static bool classof(const ExprAST* c) {
    return c->getKind() == ExprAST::Expr_BinaryOp;
  }

private:
  std::unique_ptr<ExprAST> lhs;
  std::unique_ptr<ExprAST> rhs;
  TokenKind op;
};

class UnaryExprAST : public ExprAST {
public:
  UnaryExprAST(Location location, std::unique_ptr<ExprAST> expr, TokenKind op)
      : ExprAST(ExprAST::Expr_UnaryOp, std::move(location)),
        expr(std::move(expr)), op(op) {}

  const ExprAST* getExpr() const { return expr.get(); }
  TokenKind getOp() const { return op; }

  void accept(ASTVisitor& visitor) const override {}

  // LLVM style RTTI
  static bool classof(const ExprAST* c) {
    return c->getKind() == ExprAST::Expr_UnaryOp;
  }

private:
  std::unique_ptr<ExprAST> expr;
  TokenKind op;
};

/***********************************/
/* Statement                       */
/***********************************/

class StmtAST {
public:
  enum StmtASTKind { Stmt_Simple, Stmt_If, Stmt_While, Stmt_For };

  StmtAST(StmtASTKind kind, Location location)
      : kind(kind), location(std::move(location)) {}
  virtual ~StmtAST() = default;

  StmtASTKind getKind() const { return kind; }

  const Location& loc() { return location; }

  virtual void accept(ASTVisitor& visitor) const = 0;

private:
  const StmtASTKind kind;
  Location location;
};

class StmtIfAST : public StmtAST {
public:
  StmtIfAST(Location location, std::unique_ptr<ExprAST> condition,
            std::vector<std::unique_ptr<StmtAST>> body,
            std::vector<std::unique_ptr<StmtIfAST>> elifs,
            std::vector<std::unique_ptr<StmtAST>> elseBody)
      : StmtAST(StmtASTKind::Stmt_If, std::move(location)),
        condition(std::move(condition)), body(std::move(body)),
        elifs(std::move(elifs)), elseBody(std::move(elseBody)) {}

  const ExprAST* getCondition() const { return condition.get(); }
  const std::vector<std::unique_ptr<StmtAST>>& getBody() const { return body; }
  const std::vector<std::unique_ptr<StmtIfAST>>& getElifs() const {
    return elifs;
  }
  const std::vector<std::unique_ptr<StmtAST>>& getElseBody() const {
    return elseBody;
  }

  void accept(ASTVisitor& visitor) const override {}

  /// LLVM style RTTI
  static bool classof(const StmtAST* c) {
    return c->getKind() == StmtASTKind::Stmt_If;
  }

private:
  std::unique_ptr<ExprAST> condition;
  std::vector<std::unique_ptr<StmtAST>> body;
  std::vector<std::unique_ptr<StmtIfAST>> elifs;
  std::vector<std::unique_ptr<StmtAST>> elseBody;
};

class StmtWhileAST : public StmtAST {
public:
  StmtWhileAST(Location location, std::unique_ptr<ExprAST> condition,
               std::vector<std::unique_ptr<StmtAST>> body)
      : StmtAST(StmtASTKind::Stmt_While, std::move(location)),
        condition(std::move(condition)), body(std::move(body)) {}

  const ExprAST* getCondition() const { return condition.get(); }
  const std::vector<std::unique_ptr<StmtAST>>& getBody() const { return body; }

  void accept(ASTVisitor& visitor) const override {}

  /// LLVM style RTTI
  static bool classof(const StmtAST* c) {
    return c->getKind() == StmtASTKind::Stmt_While;
  }

private:
  std::unique_ptr<ExprAST> condition;
  std::vector<std::unique_ptr<StmtAST>> body;
};

class StmtForAST : public StmtAST {
public:
  StmtForAST(Location location, std::unique_ptr<ExprAST> expr,
             std::unique_ptr<TypedVarAST> typedVar,
             std::vector<std::unique_ptr<StmtAST>> body)
      : StmtAST(StmtASTKind::Stmt_For, std::move(location)),
        expr(std::move(expr)), body(std::move(body)) {}

  const ExprAST* getExpr() const { return expr.get(); }
  const std::vector<std::unique_ptr<StmtAST>>& getBody() const { return body; }

  void accept(ASTVisitor& visitor) const override {}

  /// LLVM style RTTI
  static bool classof(const StmtAST* c) {
    return c->getKind() == StmtASTKind::Stmt_For;
  }

private:
  std::unique_ptr<ExprAST> expr;
  std::vector<std::unique_ptr<StmtAST>> body;
};

class SimpleStmtAST : public StmtAST {
public:
  enum SimpleStmtASTKind {
    SimpleStmt_Pass,
    SimpleStmt_Expr,
    SimpleStmt_Return,
    SimpleStmt_Assign
  };

  SimpleStmtAST(Location location, SimpleStmtASTKind kind)
      : StmtAST(StmtASTKind::Stmt_Simple, std::move(location)), kind(kind) {}

  SimpleStmtASTKind getKind() const { return kind; }

  void accept(ASTVisitor& visitor) const override {}

  /// LLVM style RTTI
  static bool classof(const StmtAST* c) {
    return c->getKind() == StmtASTKind::Stmt_Simple;
  }

private:
  const SimpleStmtASTKind kind;
};

class SimpleStmtPassAST : public SimpleStmtAST {
public:
  SimpleStmtPassAST(Location location)
      : SimpleStmtAST(std::move(location), SimpleStmtAST::SimpleStmt_Pass) {}

  void accept(ASTVisitor& visitor) const override {}

  /// LLVM style RTTI
  static bool classof(const SimpleStmtAST* c) {
    return c->getKind() == SimpleStmtAST::SimpleStmt_Pass;
  }
};

class SimpleStmtExprAST : public SimpleStmtAST {
public:
  SimpleStmtExprAST(Location location, std::unique_ptr<ExprAST> expr)
      : SimpleStmtAST(std::move(location), SimpleStmtAST::SimpleStmt_Expr),
        expr(std::move(expr)) {}

  const ExprAST* getExpr() const { return expr.get(); }

  void accept(ASTVisitor& visitor) const override { expr->accept(visitor); }

  /// LLVM style RTTI
  static bool classof(const SimpleStmtAST* c) {
    return c->getKind() == SimpleStmtAST::SimpleStmt_Expr;
  }

private:
  std::unique_ptr<ExprAST> expr;
};

class SimpleStmtReturnAST : public SimpleStmtAST {
public:
  SimpleStmtReturnAST(Location location, std::unique_ptr<ExprAST> expr)
      : SimpleStmtAST(std::move(location), SimpleStmtAST::SimpleStmt_Return),
        expr(std::move(expr)) {}

  const ExprAST* getExpr() const { return expr.get(); }

  void accept(ASTVisitor& visitor) const override {}

  /// LLVM style RTTI
  static bool classof(const SimpleStmtAST* c) {
    return c->getKind() == SimpleStmtAST::SimpleStmt_Return;
  }

private:
  std::unique_ptr<ExprAST> expr;
};

class SimpleStmtAssignAST : public SimpleStmtAST {
public:
  SimpleStmtAssignAST(Location location,
                      std::vector<std::unique_ptr<ExprAST>> targets,
                      std::unique_ptr<ExprAST> rhs)
      : SimpleStmtAST(std::move(location), SimpleStmtAST::SimpleStmt_Assign),
        targets(std::move(targets)), rhs(std::move(rhs)) {}

  const std::vector<std::unique_ptr<ExprAST>>& getTargets() const {
    return targets;
  }

  const ExprAST* getRhs() const { return rhs.get(); }

  void accept(ASTVisitor& visitor) const override {}

  /// LLVM style RTTI
  static bool classof(const SimpleStmtAST* c) {
    return c->getKind() == SimpleStmtAST::SimpleStmt_Assign;
  }

private:
  std::vector<std::unique_ptr<ExprAST>> targets;
  std::unique_ptr<ExprAST> rhs;
};

} // namespace chocopy

#endif // TOY_AST_H
