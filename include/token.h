#pragma once

#include <memory>
#include <string>

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
  kArrow               // ->
};

struct Location {
  std::shared_ptr<std::string> file;
  int line;
  int col;
};

struct Token {
  TokenKind kind;
  Location location;
  std::string lexeme;
};

} // namespace chocopy
