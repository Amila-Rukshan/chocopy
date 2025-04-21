#ifndef CHOCOPY_AST_H
#define CHOCOPY_AST_H

#include <llvm/ADT/StringRef.h>

#include <memory>
#include <string>
#include <vector>

namespace chocopy {

class VarDefAST;
class StmtAST;
class ExprAST;

/***********************************/
/* Program                         */
/***********************************/

class ProgramAST {
public:
  ProgramAST(std::vector<std::unique_ptr<VarDefAST>> varDefs,
             std::vector<std::unique_ptr<StmtAST>> stmts)
      : varDefs(std::move(varDefs)), stmts(std::move(stmts)) {}

  const std::vector<std::unique_ptr<VarDefAST>>& getVarDefs() const {
    return varDefs;
  }

  const std::vector<std::unique_ptr<StmtAST>>& getStmts() const {
    return stmts;
  }

private:
  std::vector<std::unique_ptr<VarDefAST>> varDefs;
  std::vector<std::unique_ptr<StmtAST>> stmts;
};

/***********************************/
/* Type                            */
/***********************************/

class TypeAST {
public:
  enum TypeASTKind { Type_Id, Type_IdString, Type_List };
  TypeAST(TypeASTKind kind, Location location)
      : kind(kind), location(std::move(location)) {}

  TypeASTKind getKind() const { return kind; }

  const Location& loc() { return location; }

private:
  const TypeASTKind kind;
  Location location;
};

class IdTypeAST : public TypeAST {
public:
  IdTypeAST(Location location, std::string id)
      : TypeAST(TypeAST::Type_Id, std::move(location)), id(std::move(id)) {}

  const llvm::StringRef getId() const { return id; }

private:
  const std::string id;
};

class IdStringTypeAST : public TypeAST {
public:
  IdStringTypeAST(Location location, std::string id)
      : TypeAST(TypeAST::Type_IdString, std::move(location)),
        id(std::move(id)) {}

  const llvm::StringRef getId() const { return id; }

private:
  const std::string id;
};

class ListTypeAST : public TypeAST {
public:
  ListTypeAST(Location location, std::unique_ptr<TypeAST> type)
      : TypeAST(TypeAST::Type_List, std::move(location)),
        type(std::move(type)) {}

  const TypeAST* getType() const { return type.get(); }

  /// LLVM style RTTI
  static bool classof(const TypeAST* c) {
    return c->getKind() == TypeAST::Type_List;
  }

private:
  std::unique_ptr<TypeAST> type;
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
  const Location& loc() { return location; }

private:
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

  LiteralASTKind getKind() const { return kind; }

  const Location& loc() { return location; }

private:
  const LiteralASTKind kind;
  Location location;
};

class LiteralNoneAST : public LiteralAST {
public:
  LiteralNoneAST(Location location)
      : LiteralAST(LiteralAST::Literal_None, std::move(location)) {}
};

class LiteralTrueAST : public LiteralAST {
public:
  LiteralTrueAST(Location location)
      : LiteralAST(LiteralAST::Literal_True, std::move(location)) {}
};

class LiteralFalseAST : public LiteralAST {
public:
  LiteralFalseAST(Location location)
      : LiteralAST(LiteralAST::Literal_False, std::move(location)) {}
};

class LiteralNumberAST : public LiteralAST {
public:
  LiteralNumberAST(Location location, int number)
      : LiteralAST(LiteralAST::Literal_Number, std::move(location)),
        number(number) {}

  int getNumber() const { return number; }

private:
  const int number;
};

class LiteralStringAST : public LiteralAST {
public:
  LiteralStringAST(Location location, std::string str)
      : LiteralAST(LiteralAST::Literal_String, std::move(location)),
        str(std::move(str)) {}

  const llvm::StringRef getStr() const { return str; }

private:
  const std::string str;
};

class LiteralIdStringAST : public LiteralAST {
public:
  LiteralIdStringAST(Location location, std::string id)
      : LiteralAST(LiteralAST::Literal_IdString, std::move(location)),
        id(std::move(id)) {}

  const llvm::StringRef getId() const { return id; }

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

private:
  std::unique_ptr<TypedVarAST> typedVar;
  std::unique_ptr<LiteralAST> literal;
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

private:
  const StmtASTKind kind;
  Location location;
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

private:
  const SimpleStmtASTKind kind;
};

class SimpleStmtPassAST : public SimpleStmtAST {
public:
  SimpleStmtPassAST(Location location)
      : SimpleStmtAST(std::move(location), SimpleStmtAST::SimpleStmt_Pass) {}
};

class SimpleStmtExprAST : public SimpleStmtAST {
public:
  SimpleStmtExprAST(Location location, std::unique_ptr<ExprAST> expr)
      : SimpleStmtAST(std::move(location), SimpleStmtAST::SimpleStmt_Expr),
        expr(std::move(expr)) {}

  const ExprAST* getExpr() const { return expr.get(); }

private:
  std::unique_ptr<ExprAST> expr;
};

class SimpleStmtReturnAST : public SimpleStmtAST {
public:
  SimpleStmtReturnAST(Location location, std::unique_ptr<ExprAST> expr)
      : SimpleStmtAST(std::move(location), SimpleStmtAST::SimpleStmt_Return),
        expr(std::move(expr)) {}

  const ExprAST* getExpr() const { return expr.get(); }

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

private:
  std::vector<std::unique_ptr<ExprAST>> targets;
  std::unique_ptr<ExprAST> rhs;
};

/***********************************/
/* Expression                      */
/***********************************/

class ExprAST {

public:
  enum ExprASTKind {
    Expr_Id,
    Expr_Mmember,
    Expr_Index,
    Expr_Literal,
    Expr_ListLiteral,
    Expr_Call,
    Expr_Print,
    Expr_Input,
    Expr_Len,
    Expr_MemberInvoke,
    Expr_BinaryOp,
    Expr_UnaryOp,
    Expr_IfElse,
  };

  ExprAST(ExprASTKind kind, Location location)
      : kind(kind), location(std::move(location)) {}
  virtual ~ExprAST() = default;

  ExprASTKind getKind() const { return kind; }

  const Location& loc() { return location; }

private:
  const ExprASTKind kind;
  Location location;
};

class IdExprAST : public ExprAST {
public:
  IdExprAST(Location location, std::string id)
      : ExprAST(ExprAST::Expr_Id, std::move(location)), id(std::move(id)) {}

  const llvm::StringRef getId() const { return id; }

private:
  const std::string id;
};

class MmemberExprAST : public ExprAST {
public:
  MmemberExprAST(Location location, std::unique_ptr<ExprAST> lhs,
                 std::string id)
      : ExprAST(ExprAST::Expr_Mmember, std::move(location)),
        lhs(std::move(lhs)), id(std::move(id)) {}

  const ExprAST* getLhs() const { return lhs.get(); }
  const llvm::StringRef getId() const { return id; }

private:
  std::unique_ptr<ExprAST> lhs;
  const std::string id;
};

class IndexExprAST : public ExprAST {
public:
  IndexExprAST(Location location, std::unique_ptr<ExprAST> lhs,
               std::unique_ptr<ExprAST> index)
      : ExprAST(ExprAST::Expr_Index, std::move(location)), lhs(std::move(lhs)),
        index(std::move(index)) {}

  const ExprAST* getLhs() const { return lhs.get(); }
  const ExprAST* getIndex() const { return index.get(); }

private:
  std::unique_ptr<ExprAST> lhs;
  std::unique_ptr<ExprAST> index;
};

class LiteralExprAST : public ExprAST {
public:
  LiteralExprAST(Location location, std::unique_ptr<LiteralAST> literal)
      : ExprAST(ExprAST::Expr_Literal, std::move(location)),
        literal(std::move(literal)) {}

  const LiteralAST* getLiteral() const { return literal.get(); }

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

private:
  std::unique_ptr<ExprAST> printArg;
};

class InputExprAST : public ExprAST {
public:
  InputExprAST(Location location, std::string id)
      : ExprAST(ExprAST::Expr_Input, std::move(location)) {}
};

class LenExprAST : public ExprAST {
public:
  LenExprAST(Location location, std::unique_ptr<ExprAST> expr)
      : ExprAST(ExprAST::Expr_Len, std::move(location)) {}
};

class MemberInvokeExprAST : public ExprAST {
public:
  MemberInvokeExprAST(Location location, std::unique_ptr<ExprAST> lhs,
                      std::string id,
                      std::vector<std::unique_ptr<ExprAST>> args)
      : ExprAST(ExprAST::Expr_MemberInvoke, std::move(location)),
        lhs(std::move(lhs)), id(std::move(id)), args(std::move(args)) {}

  const ExprAST* getLhs() const { return lhs.get(); }
  const llvm::StringRef getId() const { return id; }
  const std::vector<std::unique_ptr<ExprAST>>& getArgs() const { return args; }

private:
  std::unique_ptr<ExprAST> lhs;
  const std::string id;
  std::vector<std::unique_ptr<ExprAST>> args;
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

private:
  std::unique_ptr<ExprAST> condition;
  std::unique_ptr<ExprAST> ifBody;
  std::unique_ptr<ExprAST> elseBody;
};

class BinaryExprAST : public ExprAST {
public:
  enum BinaryOperation {
    BinaryOp_Add,
    BinaryOp_Sub,
    BinaryOp_Mul,
    BinaryOp_Div,
    BinaryOp_Mod,
    BinaryOp_And,
    BinaryOp_Or,
    BinaryOp_Eq,
    BinaryOp_Neq,
    BinaryOp_Lt,
    BinaryOp_Lteq,
    BinaryOp_Gt,
    BinaryOp_Gteq,
    BinaryOp_Is,
  };

  BinaryExprAST(Location location, std::unique_ptr<ExprAST> lhs,
                std::unique_ptr<ExprAST> rhs, BinaryOperation op)
      : ExprAST(ExprAST::Expr_BinaryOp, std::move(location)),
        lhs(std::move(lhs)), rhs(std::move(rhs)), op(op) {}

  const ExprAST* getLhs() const { return lhs.get(); }
  const ExprAST* getRhs() const { return rhs.get(); }
  BinaryOperation getOp() const { return op; }

private:
  std::unique_ptr<ExprAST> lhs;
  std::unique_ptr<ExprAST> rhs;
  BinaryOperation op;
};

class UnaryExprAST : public ExprAST {

public:
  enum UnaryOperation { UnaryOp_Neg, UnaryOp_Not };

  UnaryExprAST(Location location, std::unique_ptr<ExprAST> expr,
               UnaryOperation op)
      : ExprAST(ExprAST::Expr_UnaryOp, std::move(location)),
        expr(std::move(expr)), op(op) {}

  const ExprAST* getExpr() const { return expr.get(); }
  UnaryOperation getOp() const { return op; }

private:
  std::unique_ptr<ExprAST> expr;
  UnaryOperation op;
};

} // namespace chocopy

#endif // TOY_AST_H
