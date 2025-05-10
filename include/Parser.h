#ifndef CHOCOPY_PARSER_H
#define CHOCOPY_PARSER_H

#include <iostream>

#include "AST.h"
#include "Lexer.h"

#include "llvm/Support/Casting.h"
#include "llvm/Support/raw_ostream.h"

namespace chocopy {

/// This is a simple recursive parser for the Chocopy language. It produces a
/// well formed AST from a stream of Token supplied by the Lexer. No semantic
/// checks or symbol resolution is performed. For example, variables are
/// referenced by string and the code could reference an undeclared variable and
/// the parsing succeeds.
class Parser {
public:
  /// Create a Parser for the supplied lexer.
  Parser(Lexer& lexer) : lexer(lexer) {}

  std::unique_ptr<ProgramAST> parseProgram() {
    lexer.getNextToken();

    std::vector<std::unique_ptr<VarDefAST>> varDefs;
    std::vector<std::unique_ptr<StmtAST>> stmts;
    std::vector<std::unique_ptr<FunctionAST>> funcDefs;
    std::vector<std::unique_ptr<ClassAST>> classDefs;
    while (true) {
      Location location = lexer.getLastLocation();
      std::string id = lexer.getIdentifier();
      switch (lexer.getCurToken()) {
      case TokenKind::kIdentifier:
        if (lexer.peekNextToken() == TokenKind::kColon) {
          varDefs.push_back(parseVarDef());
        } else {
          // parse simple statements that start with an identifier
          stmts.push_back(parseStmt());
          break;
        }
        break;
      default:
        stmts.push_back(parseStmt());
        break;
      case TokenKind::k_while:
      case TokenKind::k_for:
      case TokenKind::k_if:
        stmts.push_back(parseStmt());
        break;
      case TokenKind::k_def:
        funcDefs.push_back(parseFunction());
        break;
      case TokenKind::k_class:
        classDefs.push_back(parseClass());
        break;
      case TokenKind::kEOF:
        return std::make_unique<ProgramAST>(
            std::move(varDefs), std::move(funcDefs), std::move(classDefs),
            std::move(stmts));
      }
    }
    return nullptr;
  }

private:
  Lexer& lexer;

  std::unique_ptr<ClassAST> parseClass() {
    lexer.consume(TokenKind::k_class);
    std::string id = lexer.getIdentifier();
    Location idLocation = lexer.getLastLocation();

    lexer.getNextToken();
    lexer.consume(TokenKind::kOpenParantheses);

    std::string superClassId = lexer.getIdentifier();
    Location superClassIdLocation = lexer.getLastLocation();

    lexer.getNextToken();
    lexer.consume(TokenKind::kCloseParantheses);

    lexer.consume(TokenKind::kColon);
    lexer.consume(TokenKind::kNewLine);
    lexer.consume(TokenKind::kIndent);

    if (lexer.getCurToken() == TokenKind::k_pass) {
      lexer.consume(TokenKind::k_pass);
      lexer.consume(TokenKind::kNewLine);
      lexer.consume(TokenKind::kDedent);
      return std::make_unique<ClassAST>(
          idLocation, id, superClassIdLocation, superClassId,
          std::vector<std::unique_ptr<VarDefAST>>(),
          std::vector<std::unique_ptr<FunctionAST>>());
    }

    std::vector<std::unique_ptr<FunctionAST>> funcDefs;
    std::vector<std::unique_ptr<VarDefAST>> varDefs;

    bool dclnsDone = false;

    while (lexer.getCurToken() != TokenKind::kDedent && !dclnsDone) {
      switch (lexer.getCurToken()) {
      case TokenKind::kIdentifier:
        varDefs.push_back(parseVarDef());
        break;
      case TokenKind::k_def:
        funcDefs.push_back(parseFunction());
        break;
      default:
        dclnsDone = true;
        break;
      }
    }
    lexer.consume(TokenKind::kDedent);
    return std::make_unique<ClassAST>(idLocation, id, superClassIdLocation,
                                      superClassId, std::move(varDefs),
                                      std::move(funcDefs));
  }

  std::unique_ptr<FunctionAST> parseFunction() {
    lexer.consume(TokenKind::k_def);
    std::string id = lexer.getIdentifier();
    Location idLocation = lexer.getLastLocation();
    lexer.getNextToken();

    lexer.consume(TokenKind::kOpenParantheses);
    std::vector<std::unique_ptr<TypedVarAST>> args;
    while (lexer.getCurToken() != TokenKind::kCloseParantheses) {
      std::string argId = lexer.getIdentifier();
      Location argLocation = lexer.getLastLocation();

      lexer.getNextToken();
      lexer.consume(TokenKind::kColon);
      std::unique_ptr<TypeAST> argType = parseType();
      lexer.getNextToken();
      args.push_back(std::make_unique<TypedVarAST>(argId, argLocation,
                                                   std::move(argType)));
      if (lexer.getCurToken() == TokenKind::kComma) {
        lexer.consume(TokenKind::kComma);
      }
    }
    lexer.consume(TokenKind::kCloseParantheses);

    std::unique_ptr<TypeAST> returnType = nullptr;
    if (lexer.getCurToken() == TokenKind::kColon) {
      lexer.consume(TokenKind::kColon);
    }

    if (lexer.getCurToken() == TokenKind::kArrow) {
      lexer.consume(TokenKind::kArrow);
      returnType = parseType();
      lexer.getNextToken();
      lexer.consume(TokenKind::kColon);
    } else {
      returnType =
          std::make_unique<IdTypeAST>(lexer.getLastLocation(), "<None>");
    }

    lexer.consume(TokenKind::kNewLine);
    lexer.consume(TokenKind::kIndent);

    std::vector<std::unique_ptr<VarDefAST>> varDefs;
    std::vector<std::string> globalDecls;
    std::vector<std::string> nonlocalDecls;
    std::vector<std::unique_ptr<FunctionAST>> funcDefs;
    std::vector<std::unique_ptr<StmtAST>> body;

    bool dclnsDone = false;

    while (lexer.getCurToken() != TokenKind::kDedent && !dclnsDone) {
      switch (lexer.getCurToken()) {
      case TokenKind::k_global: {
        lexer.consume(TokenKind::k_global);
        globalDecls.push_back(lexer.getIdentifier());
        lexer.getNextToken();
        lexer.consume(TokenKind::kNewLine);
        break;
      }
      case TokenKind::k_nonlocal: {
        lexer.consume(TokenKind::k_nonlocal);
        nonlocalDecls.push_back(lexer.getIdentifier());
        lexer.getNextToken();
        lexer.consume(TokenKind::kNewLine);
        break;
      }
      case TokenKind::kIdentifier: {
        if (lexer.peekNextToken() == TokenKind::kColon) {
          varDefs.push_back(parseVarDef());
        } else {
          dclnsDone = true;
        }
        break;
      }
      case TokenKind::k_def: {
        funcDefs.push_back(parseFunction());
        break;
      }
      default:
        dclnsDone = true;
        break;
      }
    }

    body.push_back(parseStmt());
    while (lexer.getCurToken() != TokenKind::kDedent) {
      auto stmt = parseStmt();
      if (stmt) {
        body.push_back(std::move(stmt));
      }
    }

    lexer.consume(TokenKind::kDedent);
    return std::make_unique<FunctionAST>(
        idLocation, id, std::move(args), std::move(returnType),
        std::move(varDefs), std::move(globalDecls), std::move(nonlocalDecls),
        std::move(funcDefs), std::move(body));
  }

  std::unique_ptr<StmtAST> parseStmt() {
    switch (lexer.getCurToken()) {
    case TokenKind::k_while:
      return parseWhileStmt();
      break;
    case TokenKind::k_for:
      return parseForStmt();
      break;
    case TokenKind::k_if:
      return parseIfStmt();
      break;
    default:
      return parseSimpleStmt();
    }
    return nullptr;
  }

  std::vector<std::unique_ptr<StmtAST>> parseBlock() {
    std::vector<std::unique_ptr<StmtAST>> stmts;
    lexer.consume(TokenKind::kNewLine);
    lexer.consume(TokenKind::kIndent);
    while (lexer.getCurToken() != TokenKind::kDedent) {
      auto stmt = parseStmt();
      stmts.push_back(std::move(stmt));
    }
    lexer.consume(TokenKind::kDedent);
    return stmts;
  }

  std::unique_ptr<StmtWhileAST> parseWhileStmt() {
    lexer.consume(TokenKind::k_while);
    std::unique_ptr<ExprAST> condition = parseExpr();
    if (!condition) {
      return parseError<StmtWhileAST>("expression", "after 'while'");
    }
    lexer.consume(TokenKind::kColon);
    std::vector<std::unique_ptr<StmtAST>> body = parseBlock();
    return std::make_unique<StmtWhileAST>(
        lexer.getLastLocation(), std::move(condition), std::move(body));
  }

  std::unique_ptr<StmtForAST> parseForStmt() {
    lexer.consume(TokenKind::k_for);
    std::string id = lexer.getIdentifier();
    Location idLocation = lexer.getLastLocation();
    lexer.getNextToken();
    lexer.consume(TokenKind::k_in);
    std::unique_ptr<ExprAST> expr = parseExpr();
    if (!expr) {
      return parseError<StmtForAST>("expression", "after 'for'");
    }
    lexer.consume(TokenKind::kColon);
    std::vector<std::unique_ptr<StmtAST>> body = parseBlock();
    return std::make_unique<StmtForAST>(
        lexer.getLastLocation(), std::move(expr),
        std::make_unique<TypedVarAST>(id, idLocation, nullptr),
        std::move(body));
  }

  std::unique_ptr<StmtIfAST> parseIfStmt() {
    lexer.consume(TokenKind::k_if);
    std::unique_ptr<ExprAST> condition = parseExpr();
    if (!condition) {
      return parseError<StmtIfAST>("expression", "after 'if'");
    }
    lexer.consume(TokenKind::kColon);
    std::vector<std::unique_ptr<StmtAST>> body = parseBlock();

    // check for elif(s)
    std::vector<std::unique_ptr<StmtIfAST>> elifs;
    if (lexer.getCurToken() == TokenKind::k_elif) {
      while (lexer.getCurToken() == TokenKind::k_elif) {
        lexer.consume(TokenKind::k_elif);
        std::unique_ptr<ExprAST> elifCondition = parseExpr();
        if (!elifCondition) {
          return parseError<StmtIfAST>("expression", "after 'elif'");
        }
        lexer.consume(TokenKind::kColon);
        std::vector<std::unique_ptr<StmtAST>> elifBody = parseBlock();
        elifs.push_back(std::make_unique<StmtIfAST>(
            lexer.getLastLocation(), std::move(elifCondition),
            std::move(elifBody), std::vector<std::unique_ptr<StmtIfAST>>{},
            std::vector<std::unique_ptr<StmtAST>>{}));
      }
    }

    // check for else
    std::vector<std::unique_ptr<StmtAST>> elseBody;
    if (lexer.getCurToken() == TokenKind::k_else) {
      lexer.getNextToken();
      lexer.consume(TokenKind::kColon);
      elseBody = parseBlock();
    }

    return std::make_unique<StmtIfAST>(lexer.getLastLocation(),
                                       std::move(condition), std::move(body),
                                       std::move(elifs), std::move(elseBody));
  }

  std::unique_ptr<SimpleStmtAST> parseSimpleStmt() {
    switch (lexer.getCurToken()) {
    case TokenKind::k_pass:
      lexer.getNextToken();
      lexer.consume(TokenKind::kNewLine);
      return std::make_unique<SimpleStmtPassAST>(lexer.getLastLocation());
    case TokenKind::k_return: {
      lexer.consume(TokenKind::k_return);
      if (lexer.getCurToken() == TokenKind::kNewLine) {
        lexer.consume(TokenKind::kNewLine);
        return std::make_unique<SimpleStmtReturnAST>(lexer.getLastLocation(),
                                                     nullptr);
      }
      Location location = lexer.getLastLocation();
      std::unique_ptr<ExprAST> retExpr = nullptr;
      retExpr = parseExpr();
      lexer.consume(TokenKind::kNewLine);
      return std::make_unique<SimpleStmtReturnAST>(location,
                                                   std::move(retExpr));
    }
    default:
      std::unique_ptr<ExprAST> expr = parseExpr();
      if (lexer.getCurToken() == TokenKind::kNewLine) {
        lexer.consume(TokenKind::kNewLine);
        return std::make_unique<SimpleStmtExprAST>(
            Location{std::make_shared<std::string>(""), 0, 0}, std::move(expr));
      }
      std::vector<std::unique_ptr<ExprAST>> targets;
      targets.push_back(std::move(expr));
      std::unique_ptr<ExprAST> rightExp = nullptr;
      while (lexer.getCurToken() == TokenKind::kAssign) {
        lexer.consume(TokenKind::kAssign);
        std::unique_ptr<ExprAST> rhs = parseExpr();
        if (!rhs) {
          return parseError<SimpleStmtAST>("expression", "after '='");
        }
        if (lexer.getCurToken() == TokenKind::kNewLine) {
          rightExp = std::move(rhs);
        } else {
          targets.push_back(std::move(rhs));
        }
      }
      lexer.consume(TokenKind::kNewLine);
      return std::make_unique<SimpleStmtAssignAST>(
          lexer.getLastLocation(), std::move(targets), std::move(rightExp));
      break;
    }
    return nullptr;
  }

  std::unique_ptr<ExprAST> parseExpr() {
    if (lexer.getCurToken() == TokenKind::k_not) {
      lexer.getNextToken();
      Location notLocation = lexer.getLastLocation();
      auto expr = parseExpr();
      if (!expr) {
        return parseError<ExprAST>("expression", "after 'not'");
      }
      return std::make_unique<UnaryExprAST>(notLocation, std::move(expr),
                                            TokenKind::k_not);
    }

    auto lhs = parseCExpr();
    if (!lhs) {
      return nullptr;
    }

    if (lexer.getCurToken() == TokenKind::kPlus ||
        lexer.getCurToken() == TokenKind::kMinus ||
        lexer.getCurToken() == TokenKind::kMul ||
        lexer.getCurToken() == TokenKind::kIntDiv ||
        lexer.getCurToken() == TokenKind::kMod ||
        lexer.getCurToken() == TokenKind::k_is ||
        lexer.getCurToken() == TokenKind::kEqual ||
        lexer.getCurToken() == TokenKind::kInEqual ||
        lexer.getCurToken() == TokenKind::kLessThan ||
        lexer.getCurToken() == TokenKind::kGreaterThan ||
        lexer.getCurToken() == TokenKind::kLessThanOrEqual ||
        lexer.getCurToken() == TokenKind::kGreaterThanOrEqual ||
        lexer.getCurToken() == TokenKind::kAttrAccessOp ||
        lexer.getCurToken() == TokenKind::kOpenSquareBracket) {
      return parseBinaryOpRHS(0, std::move(lhs));
    }

    while (lexer.getCurToken() == TokenKind::k_and ||
           lexer.getCurToken() == TokenKind::k_or) {
      TokenKind op = lexer.getCurToken();
      Location opLocation = lexer.getLastLocation();
      lexer.getNextToken();
      auto rhs = parseExpr();
      if (!rhs) {
        return parseError<ExprAST>("expression", "after binary operator");
      }
      lhs = std::make_unique<BinaryExprAST>(opLocation, std::move(lhs),
                                            std::move(rhs), op);
    }

    if (lexer.getCurToken() == TokenKind::k_if) {
      lexer.getNextToken();
      auto condition = parseExpr();
      if (!condition) {
        return parseError<ExprAST>("expression", "after 'if'");
      }
      lexer.consume(TokenKind::k_else);
      auto elseExpr = parseExpr();
      if (!elseExpr) {
        return parseError<ExprAST>("expression", "after 'else'");
      }
      lhs = std::make_unique<IfElseExprAST>(
          lexer.getLastLocation(), std::move(lhs), std::move(condition),
          std::move(elseExpr));
    }

    return lhs;
  }

  std::unique_ptr<ExprAST> parseCExpr() {
    std::string id;
    Location idLocation;
    switch (lexer.getCurToken()) {
    case TokenKind::kMinus: {
      TokenKind op = lexer.getCurToken();
      Location opLocation = lexer.getLastLocation();
      lexer.getNextToken();
      auto rhs = parseCExpr();
      if (!rhs) {
        return parseError<ExprAST>("expression", "after unary operator");
      }
      return std::make_unique<UnaryExprAST>(opLocation, std::move(rhs), op);
    }
    case TokenKind::kIdentifier:
      id = lexer.getIdentifier();
      idLocation = lexer.getLastLocation();
      lexer.getNextToken();

      // Handle function calls
      if (lexer.getCurToken() == TokenKind::kOpenParantheses) {
        lexer.getNextToken();
        std::vector<std::unique_ptr<ExprAST>> args;
        while (lexer.getCurToken() != TokenKind::kCloseParantheses) {
          auto arg = parseExpr();
          if (!arg) {
            return parseError<ExprAST>("expression", "in function call");
          }
          args.push_back(std::move(arg));
          if (lexer.getCurToken() == TokenKind::kComma) {
            lexer.getNextToken();
          } else {
            break;
          }
        }
        lexer.consume(TokenKind::kCloseParantheses);
        return std::make_unique<CallExprAST>(
            idLocation, std::make_unique<IdExprAST>(idLocation, id),
            std::move(args));
      }

      // Simple identifier
      return std::make_unique<IdExprAST>(lexer.getLastLocation(), id);
    case TokenKind::kIntegerLiteral:
    case TokenKind::kStringLiteral:
    case TokenKind::k_True:
    case TokenKind::k_False:
    case TokenKind::k_None:
      return std::make_unique<LiteralExprAST>(lexer.getLastLocation(),
                                              parseLiteral());
    case TokenKind::kOpenParantheses: {
      lexer.getNextToken();
      auto expr = parseExpr();
      if (!expr) {
        return parseError<ExprAST>("expression", "after '('");
      }
      lexer.consume(TokenKind::kCloseParantheses);
      return expr;
    }
    case TokenKind::kOpenSquareBracket: {
      lexer.getNextToken();
      std::vector<std::unique_ptr<ExprAST>> elements;
      while (lexer.getCurToken() != TokenKind::kCloseSquareBracket) {
        auto element = parseExpr();
        if (!element) {
          return parseError<ExprAST>("expression", "in list literal");
        }
        elements.push_back(std::move(element));
        if (lexer.getCurToken() == TokenKind::kComma) {
          lexer.getNextToken();
        } else {
          break;
        }
      }
      lexer.consume(TokenKind::kCloseSquareBracket);
      return std::make_unique<ListLiteralExprAST>(lexer.getLastLocation(),
                                                  std::move(elements));
    }
    default:

      return parseError<ExprAST>("expression", "when expecting a cexpr");
    }
  }

  std::unique_ptr<ExprAST> parseBinaryOpRHS(int precedence,
                                            std::unique_ptr<ExprAST> lhs) {
    while (true) {
      int tokenPrecedence = getTokenPrecedence(lexer.getCurToken());
      if (tokenPrecedence < precedence) {
        return lhs;
      }

      TokenKind op = lexer.getCurToken();
      Location opLocation = lexer.getLastLocation();
      lexer.getNextToken();

      auto rhs = parseCExpr();

      // consume close square bracket if op is open square bracket
      if (op == TokenKind::kOpenSquareBracket) {
        lexer.consume(TokenKind::kCloseSquareBracket);
        op = TokenKind::kIndexAccessOp;
      }

      if (!rhs) {
        return nullptr;
      }

      int nextPrecedence = getTokenPrecedence(lexer.getCurToken());
      if (tokenPrecedence < nextPrecedence) {
        rhs = parseBinaryOpRHS(tokenPrecedence + 1, std::move(rhs));
        if (!rhs) {
          return nullptr;
        }
      }

      lhs = std::make_unique<BinaryExprAST>(opLocation, std::move(lhs),
                                            std::move(rhs), op);
    }
  }

  std::unique_ptr<VarDefAST> parseVarDef() {
    lexer.getNextToken();
    std::string id = lexer.getIdentifier();
    Location idLocation = lexer.getLastLocation();

    lexer.consume(TokenKind::kColon);
    std::unique_ptr<TypeAST> type = parseType();
    if (!type) {
      return parseError<VarDefAST>("type", "after identifier");
    }
    lexer.getNextToken();
    lexer.consume(TokenKind::kAssign);
    std::unique_ptr<LiteralAST> literal = parseLiteral();
    if (!literal) {
      return parseError<VarDefAST>("literal", "after type");
    }
    lexer.consume(TokenKind::kNewLine);
    return std::make_unique<VarDefAST>(
        std::make_unique<TypedVarAST>(std::move(id), idLocation,
                                      std::move(type)),
        std::move(literal));
  }

  std::unique_ptr<TypeAST> parseType() {
    std::unique_ptr<TypeAST> type;
    // track the number of dimensions for list types
    static int dimension = 0;

    switch (lexer.getCurToken()) {
    case TokenKind::kIdentifier:
      return std::make_unique<IdTypeAST>(lexer.getLastLocation(),
                                         lexer.getIdentifier());
    case TokenKind::kStringLiteral:
      return std::make_unique<IdStringTypeAST>(lexer.getLastLocation(),
                                               lexer.getStringLiteral());
    case TokenKind::kOpenSquareBracket:
      lexer.getNextToken();
      dimension += 1;
      type = parseType();
      if (type) {
        lexer.getNextToken();
        if (lexer.getCurToken() != TokenKind::kCloseSquareBracket) {
          return parseError<TypeAST>("]", "after type");
        }
        auto listType = llvm::dyn_cast<ListTypeAST>(type.get());
        if (listType) {
          return type;
        }
        int dimensionTemp = dimension;
        dimension = 0;
        return std::make_unique<ListTypeAST>(lexer.getLastLocation(),
                                             std::move(type), dimensionTemp);
      }
      break;
    default:
      return parseError<TypeAST>("identifier or string literal or list type",
                                 "when expecting a type");
      break;
    }

    return nullptr;
  }

  std::unique_ptr<LiteralAST> parseLiteral() {
    Location location = lexer.getLastLocation();
    int intValue = 0;
    std::string strValue;
    std::string idValue;
    switch (lexer.getCurToken()) {
    case TokenKind::kIntegerLiteral:
      intValue = lexer.getIntegerValue();
      lexer.getNextToken();
      return std::make_unique<LiteralNumberAST>(location, intValue);
    case TokenKind::kStringLiteral:
      strValue = lexer.getStringLiteral();
      lexer.getNextToken();
      return std::make_unique<LiteralStringAST>(location, strValue);
    case TokenKind::kIdentifier:
      idValue = lexer.getIdentifier();
      lexer.getNextToken();
      return std::make_unique<LiteralIdStringAST>(location, idValue);
    case TokenKind::k_True:
      lexer.getNextToken();
      return std::make_unique<LiteralTrueAST>(location);
    case TokenKind::k_False:
      lexer.getNextToken();
      return std::make_unique<LiteralFalseAST>(location);
    case TokenKind::k_None:
      lexer.getNextToken();
      return std::make_unique<LiteralNoneAST>(location);
    default:
      llvm::errs() << "unknown token '" << lexer.getCurToken()
                   << "' when expecting a literal\n";
      return nullptr;
    }
  }

  int getTokenPrecedence(TokenKind tokenKind) {
    switch (tokenKind) {
    case TokenKind::k_or:
      return 2;
    case TokenKind::k_and:
      return 3;
    case TokenKind::kEqual:
    case TokenKind::kInEqual:
    case TokenKind::kLessThan:
    case TokenKind::kGreaterThan:
    case TokenKind::kLessThanOrEqual:
    case TokenKind::kGreaterThanOrEqual:
    case TokenKind::k_is:
      return 5;
    case TokenKind::kPlus:
    case TokenKind::kMinus:
      return 6;
    case TokenKind::kMul:
    case TokenKind::kIntDiv:
    case TokenKind::kMod:
      return 7;
    case TokenKind::kAttrAccessOp:
    case TokenKind::kOpenSquareBracket:
      return 9;
    default:
      return -1;
    }
  }

  /// Helper function to signal errors while parsing, it takes an argument
  /// indicating the expected token and another argument giving more context.
  /// Location is retrieved from the lexer to enrich the error message.
  /// Gracefully returns nullptr to signal the error.
  template <typename R, typename T, typename U = const char*>
  std::unique_ptr<R> parseError(T&& expected, U&& context = "") {
    auto curToken = lexer.getCurToken();
    llvm::errs() << "Parse error (" << lexer.getLastLocation().line << ", "
                 << lexer.getLastLocation().col << "): expected '" << expected
                 << "' " << context << " but has Token "
                 << " '" << tokenKindToString(curToken) << "'"
                 << "\n";
    return nullptr;
  }
};

} // namespace chocopy

#endif // CHOCOPY_PARSER_H
