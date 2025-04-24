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
    while (true) {
      Location location = lexer.getLastLocation();
      std::string id = lexer.getIdentifier();
      switch (lexer.getCurToken()) {
      case TokenKind::kIdentifier:
        if (lexer.peekNextToken() == TokenKind::kColon) {
          varDefs.push_back(parseVarDef());
        } else {
          // parse simple statements that start with an identifier
          stmts.push_back(parseSimpleStmt());
          break;
        }
        break;
      case TokenKind::k_pass:
      case TokenKind::k_return:
        stmts.push_back(parseSimpleStmt());
        break;
      case TokenKind::k_for:
        // parse for loop
      case TokenKind::k_while:
        // parse while loop
      case TokenKind::k_if:
        // parse if statement
      case TokenKind::k_def:
        // parse function definition
      case TokenKind::k_class:
        // parse class definition
      default:
        lexer.getNextToken();
        break;
      case TokenKind::kEOF:
        return std::make_unique<ProgramAST>(std::move(varDefs),
                                            std::move(stmts));
      }
    }
    return nullptr;
  }

private:
  Lexer& lexer;

  std::unique_ptr<SimpleStmtAST> parseSimpleStmt() {
    switch (lexer.getCurToken()) {
    case TokenKind::k_pass:
      lexer.getNextToken();
      lexer.consume(TokenKind::kNewLine);
      return std::make_unique<SimpleStmtPassAST>(lexer.getLastLocation());
    case TokenKind::k_return:
      if (lexer.peekNextToken() == TokenKind::kNewLine) {
        lexer.getNextToken();
        lexer.consume(TokenKind::kNewLine);
        return std::make_unique<SimpleStmtReturnAST>(lexer.getLastLocation(),
                                                     nullptr);
      }
    default:
      std::unique_ptr<ExprAST> expr = parseExpr();
      if (lexer.getCurToken() == TokenKind::kNewLine) {
        lexer.consume(TokenKind::kNewLine);
        return std::make_unique<SimpleStmtExprAST>(Location{0, 0},
                                                   std::move(expr));
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
    if (lexer.peekNextToken() == TokenKind::k_not) {
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
        lexer.getCurToken() == TokenKind::kGreaterThanOrEqual) {
      return std::move(parseBinaryOpRHS(0, std::move(lhs)));
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

      // Handle member access
      if (lexer.getCurToken() == TokenKind::kAttrAccessOp) {
        lexer.getNextToken();
        std::string member = lexer.getIdentifier();
        lexer.getNextToken();
        return std::make_unique<MmemberExprAST>(
            lexer.getLastLocation(),
            std::make_unique<IdExprAST>(lexer.getLastLocation(), id), member);
      }

      // Handle index access
      if (lexer.getCurToken() == TokenKind::kOpenSquareBracket) {
        lexer.getNextToken();
        auto index = parseExpr();
        if (!index) {
          return parseError<ExprAST>("expression", "in index access");
        }
        lexer.consume(TokenKind::kCloseSquareBracket);
        return std::make_unique<IndexExprAST>(
            lexer.getLastLocation(),
            std::make_unique<IdExprAST>(idLocation, id), std::move(index));
      }

      // Simple identifier
      return std::make_unique<IdExprAST>(lexer.getLastLocation(), id);
    case TokenKind::kIntegerLiteral:
    case TokenKind::kStringLiteral:
    case TokenKind::k_True:
    case TokenKind::k_False:
    case TokenKind::k_None:
      return std::make_unique<LiteralExprAST>(lexer.getLastLocation(),
                                              std::move(parseLiteral()));
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

    lexer.getNextToken();
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
