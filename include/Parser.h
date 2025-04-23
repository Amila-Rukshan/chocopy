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
      break;
    }
    return nullptr;
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
    lexer.getNextToken();
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
    switch (lexer.getCurToken()) {
    case TokenKind::kIntegerLiteral:
      return std::make_unique<LiteralNumberAST>(lexer.getLastLocation(),
                                                lexer.getIntegerValue());
    case TokenKind::kStringLiteral:
      return std::make_unique<LiteralStringAST>(lexer.getLastLocation(),
                                                lexer.getStringLiteral());
    case TokenKind::kIdentifier:
      return std::make_unique<LiteralIdStringAST>(lexer.getLastLocation(),
                                                  lexer.getIdentifier());
    case TokenKind::k_True:
      return std::make_unique<LiteralTrueAST>(lexer.getLastLocation());
    case TokenKind::k_False:
      return std::make_unique<LiteralFalseAST>(lexer.getLastLocation());
    case TokenKind::k_None:
      return std::make_unique<LiteralNoneAST>(lexer.getLastLocation());
    default:
      llvm::errs() << "unknown token '" << lexer.getCurToken()
                   << "' when expecting a literal\n";
      return nullptr;
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
