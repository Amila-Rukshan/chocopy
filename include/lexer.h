#ifndef CHOCOPY_LEXER_H
#define CHOCOPY_LEXER_H

#include <memory>

#include "token.h"

#include "llvm/ADT/StringRef.h"

namespace chocopy {

class Lexer {
public:
  Lexer(std::string filename)
      : lastLocation(
            {std::make_shared<std::string>(std::move(filename)), 0, 0}) {}
  virtual ~Lexer() = default;

private:
  /// Delegate to a derived class fetching the next line. Returns an empty
  /// string to signal end of file (EOF). Lines are expected to always finish
  /// with "\n"
  virtual llvm::StringRef readNextLine() = 0;

  Location lastLocation;
  int curLineNum = 0;
  int curCol = 0;
  llvm::StringRef curLineBuffer = "\n";
};

class LexerBuffer final : public Lexer {
public:
  LexerBuffer(const char* begin, const char* end, std::string filename)
      : Lexer(std::move(filename)), current(begin), end(end) {}

private:
  llvm::StringRef readNextLine() override {
    auto* begin = current;
    while (current <= end && *current && *current != '\n')
      ++current;
    if (current <= end && *current)
      ++current;
    llvm::StringRef result{begin, static_cast<size_t>(current - begin)};
    return result;
  }
  const char *current, *end;
};

} // namespace chocopy

#endif