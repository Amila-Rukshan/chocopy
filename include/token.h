#pragma once

#include <memory>
#include <string>
#include <unordered_map>

namespace chocopy {

enum TokenKind {
  // indentation tokens
  kNewLine,
  kIndent,
  kDedent,

  // keywords
  k_False,    // False
  k_None,     // None
  k_True,     // True
  k_and,      // and
  k_as,       // as
  k_assert,   // assert
  k_async,    // async
  k_await,    // await
  k_break,    // break
  k_class,    // class
  k_continue, // continue
  k_def,      // def
  k_del,      // del
  k_elif,     // elif
  k_else,     // else
  k_except,   // except
  k_finally,  // finally,
  k_for,      // for
  k_from,     // from
  k_global,   // global
  k_if,       // if
  k_import,   // import
  k_in,       // in
  k_is,       // is
  k_lambda,   // lambda
  k_nonlocal, // nonlocal
  k_not,      // not
  k_or,       // or
  k_pass,     // pass
  k_raise,    // raise
  k_return,   // return
  k_try,      // try
  k_while,    // while
  k_with,     // with
  k_yield,    // yield

  kIdentifier,
  kIntegerLiteral,
  kStringLiteral,

  kPlus,               // +
  kMinus,              // -
  kMul,                // *
  kIntDiv,             // //
  kMod,                // %
  kLessThan,           // <
  kGreaterThan,        // >
  kLessThanOrEqual,    // <=
  kGreaterThanOrEqual, // >=
  kEqual,              // ==
  kInEqual,            // !=
  kAssign,             // =
  kOpenParantheses,    // (
  kCloseParantheses,   // )
  kOpenSquareBracket,  // [
  kCloseSquareBracket, // ]
  kComma,              // ,
  kColon,              // :
  kAttrAccessOp,       // .
  kArrow,              // ->

  kEOF,           // end of file
  kUnknown,       // unknown token
  kInvalidIndent, // invalid indent token
};

std::string tokenKindToString(TokenKind kind) {
  static const std::unordered_map<TokenKind, std::string> tokenKindMap = {
      {TokenKind::k_False, "k_False"},
      {TokenKind::k_None, "k_None"},
      {TokenKind::k_True, "k_True"},
      {TokenKind::k_and, "k_and"},
      {TokenKind::k_as, "k_as"},
      {TokenKind::k_assert, "k_assert"},
      {TokenKind::k_async, "k_async"},
      {TokenKind::k_await, "k_await"},
      {TokenKind::k_break, "k_break"},
      {TokenKind::k_class, "k_class"},
      {TokenKind::k_continue, "k_continue"},
      {TokenKind::k_def, "k_def"},
      {TokenKind::k_del, "k_del"},
      {TokenKind::k_elif, "k_elif"},
      {TokenKind::k_else, "k_else"},
      {TokenKind::k_except, "k_except"},
      {TokenKind::k_finally, "k_finally"},
      {TokenKind::k_for, "k_for"},
      {TokenKind::k_from, "k_from"},
      {TokenKind::k_global, "k_global"},
      {TokenKind::k_if, "k_if"},
      {TokenKind::k_import, "k_import"},
      {TokenKind::k_in, "k_in"},
      {TokenKind::k_is, "k_is"},
      {TokenKind::k_lambda, "k_lambda"},
      {TokenKind::k_nonlocal, "k_nonlocal"},
      {TokenKind::k_not, "k_not"},
      {TokenKind::k_or, "k_or"},
      {TokenKind::k_pass, "k_pass"},
      {TokenKind::k_raise, "k_raise"},
      {TokenKind::k_return, "k_return"},
      {TokenKind::k_try, "k_try"},
      {TokenKind::k_while, "k_while"},
      {TokenKind::k_with, "k_with"},
      {TokenKind::k_yield, "k_yield"},
      {TokenKind::kIdentifier, "kIdentifier"},
      {TokenKind::kIntegerLiteral, "kIntegerLiteral"},
      {TokenKind::kStringLiteral, "kStringLiteral"},
      {TokenKind::kPlus, "kPlus"},
      {TokenKind::kMinus, "kMinus"},
      {TokenKind::kMul, "kMul"},
      {TokenKind::kIntDiv, "kIntDiv"},
      {TokenKind::kMod, "kMod"},
      {TokenKind::kLessThan, "kLessThan"},
      {TokenKind::kGreaterThan, "kGreaterThan"},
      {TokenKind::kLessThanOrEqual, "kLessThanOrEqual"},
      {TokenKind::kGreaterThanOrEqual, "kGreaterThanOrEqual"},
      {TokenKind::kEqual, "kEqual"},
      {TokenKind::kInEqual, "kInEqual"},
      {TokenKind::kAssign, "kAssign"},
      {TokenKind::kOpenParantheses, "kOpenParantheses"},
      {TokenKind::kCloseParantheses, "kCloseParantheses"},
      {TokenKind::kOpenSquareBracket, "kOpenSquareBracket"},
      {TokenKind::kCloseSquareBracket, "kCloseSquareBracket"},
      {TokenKind::kComma, "kComma"},
      {TokenKind::kColon, "kColon"},
      {TokenKind::kAttrAccessOp, "kAttrAccessOp"},
      {TokenKind::kArrow, "kArrow"},
      {TokenKind::kNewLine, "kNewLine"},
      {TokenKind::kIndent, "kIndent"},
      {TokenKind::kDedent, "kDedent"},
      {TokenKind::kEOF, "kEOF"},
      {TokenKind::kUnknown, "kUnknown"},
      {TokenKind::kInvalidIndent, "kInvalidIndent"},
  };

  auto it = tokenKindMap.find(kind);
  return it != tokenKindMap.end() ? it->second : "Unknown TokenKind";
}

struct Location {
  std::shared_ptr<std::string> file;
  int line;
  int col;
};

} // namespace chocopy
