#ifndef CHOCOPY_LEXER_H
#define CHOCOPY_LEXER_H

#include <algorithm>
#include <array>
#include <string>

#include "Token.h"

#include "llvm/ADT/StringRef.h"
#include "llvm/Support/raw_ostream.h"

namespace chocopy {

constexpr uint8_t kIndentSize = 4;

class Lexer {
public:
  Lexer(std::string filename)
      : lastLocation(
            {std::make_shared<std::string>(std::move(filename)), 0, 0}) {}

  virtual ~Lexer() = default;

  TokenKind getCurToken() { return curTok; }

  TokenKind getNextToken() { return curTok = getToken(); }

  TokenKind peekNextToken(size_t lookahead = 1) {
    LexerState savedState = saveState();
    TokenKind tokenKind = TokenKind::kUnknown;
    for (size_t i = 0; i < lookahead; ++i) {
      tokenKind = getToken();
    }
    restoreState(savedState);
    return tokenKind;
  }

  void consume(TokenKind tok) {
    if (tok != curTok) {
      llvm::errs() << "Token mismatch: expected " << tokenKindToString(tok)
                   << ", but got " << tokenKindToString(curTok) << "\n";
      llvm::errs() << *lastLocation.file << ":" << lastLocation.line << ":"
                   << lastLocation.col << "\n";
    }
    assert(tok == curTok && "consume Token mismatch expectation");
    getNextToken();
  }

  Location getLastLocation() { return lastLocation; }

  std::string getIdentifier() { return identifierStr; }

  int64_t getIntegerValue() { return intValue; }

  std::string getStringLiteral() { return strLiteral; }

private:
  /// Delegate to a derived class fetching the next line. Returns an empty
  /// string to signal end of file (EOF). Lines are expected to always finish
  /// with "\n"
  virtual llvm::StringRef readNextLine() = 0;

  int getNextChar() {
    // The current line buffer should not be empty unless it is the end of file.
    if (curLineBuffer.empty())
      return EOF;
    ++curCol;
    auto nextchar = curLineBuffer.front();
    curLineBuffer = curLineBuffer.drop_front();
    if (curLineBuffer.empty()) {
      curLineBuffer = readNextLine();
    }
    if (nextchar == '\n') {
      ++curLineNum;
      curCol = 0;
    }
    return nextchar;
  }

  TokenKind getToken() {

    if (lastChar == '\0') {
      lastChar = getNextChar();
    }

    if (lastChar == '\n' && curLineNum == 1 && curCol == 0) {
      lastChar = getNextChar();
      while (isspace(lastChar)) {
        lastChar = getNextChar();
      }
    }

    if (processingLeadingSpaces) {
      // count leading spaces
      uint16_t leadingSpacesCount = 0;
      while (lastChar == ' ') {
        ++leadingSpacesCount;
        lastChar = getNextChar();
      }
      if (lastChar == '\n') {
        lastChar = getNextChar();
        return getToken();
      }
      if (lastChar == '#') {
        // comment until end of line.
        do {
          lastChar = getNextChar();
        } while (lastChar != EOF && lastChar != '\n');

        if (lastChar != EOF)
          return getToken();
      }
      currentLineIndentLevel = leadingSpacesCount / kIndentSize;
      processingLeadingSpaces = false;
    }

    if (isLogicalLine && !indentDenentDone && !processingLeadingSpaces) {

      if ((currentLineIndentLevel - prevLineIndentLevel) > 1) {
        // invalid indent
        return TokenKind::kInvalidIndent;
      }
      if ((currentLineIndentLevel - prevLineIndentLevel) == 1) {
        prevLineIndentLevel = currentLineIndentLevel;
        currentLineIndentLevel = 0;
        indentDenentDone = true;
        return TokenKind::kIndent;
      }
      if (currentLineIndentLevel < prevLineIndentLevel) {
        prevLineIndentLevel -= 1;
        if (prevLineIndentLevel == currentLineIndentLevel) {
          prevLineIndentLevel = currentLineIndentLevel;
          currentLineIndentLevel = 0;
          indentDenentDone = true;
        }
        return TokenKind::kDedent;
      }
      currentLineIndentLevel = 0;
      indentDenentDone = true;
    }

    // return any temporary saved token before generating indent token
    if (indentDenentDone &&
        firstTokenInIndentedLogicalLine != TokenKind::kUnknown) {
      TokenKind savedToken = firstTokenInIndentedLogicalLine;
      firstTokenInIndentedLogicalLine = TokenKind::kUnknown;
      return savedToken;
    }

    TokenKind tokenKind = findToken();

    if (!isLogicalLine && tokenKind != TokenKind::kUnknown) {
      isLogicalLine = true;
    }

    if (tokenKind <= 58 && isLogicalLine && !indentDenentDone) {
      // save the previous token kind before sending the indent token
      firstTokenInIndentedLogicalLine = tokenKind;
      // return indent/dedent tokens
      return getToken();
    }

    return tokenKind;
  }

  TokenKind findToken() {

    // ignore spaces non leading spaces
    while (isspace(lastChar) && lastChar != '\n')
      lastChar = getNextChar();

    lastLocation.line = curLineNum;
    lastLocation.col = curCol;

    if (lastChar == '\n') {
      processingLeadingSpaces = true;
      indentDenentDone = false;
      lastChar = getNextChar();
      if (isLogicalLine) {
        isLogicalLine = false;
        return TokenKind::kNewLine;
      }
    }

    if (lastChar == '\0') {
      lastChar = getNextChar();
    }

    // Identifier: [a-zA-Z][a-zA-Z0-9_]*
    if (isalpha(lastChar) || lastChar == '_') {
      identifierStr = lastChar;
      lastChar = getNextChar();
      while (isalnum(lastChar) || lastChar == '_') {
        identifierStr += lastChar;
        lastChar = getNextChar();
      }

      if (identifierStr == "False")
        return TokenKind::k_False;
      else if (identifierStr == "None")
        return TokenKind::k_None;
      if (identifierStr == "True")
        return TokenKind::k_True;
      if (identifierStr == "and")
        return TokenKind::k_and;
      if (identifierStr == "as")
        return TokenKind::k_as;
      if (identifierStr == "assert")
        return TokenKind::k_assert;
      if (identifierStr == "async")
        return TokenKind::k_async;
      if (identifierStr == "await")
        return TokenKind::k_await;
      if (identifierStr == "break")
        return TokenKind::k_break;
      if (identifierStr == "class")
        return TokenKind::k_class;
      if (identifierStr == "continue")
        return TokenKind::k_continue;
      if (identifierStr == "def")
        return TokenKind::k_def;
      if (identifierStr == "del")
        return TokenKind::k_del;
      if (identifierStr == "elif")
        return TokenKind::k_elif;
      if (identifierStr == "else")
        return TokenKind::k_else;
      if (identifierStr == "except")
        return TokenKind::k_except;
      if (identifierStr == "finally")
        return TokenKind::k_finally;
      if (identifierStr == "for")
        return TokenKind::k_for;
      if (identifierStr == "from")
        return TokenKind::k_from;
      if (identifierStr == "global")
        return TokenKind::k_global;
      if (identifierStr == "if")
        return TokenKind::k_if;
      if (identifierStr == "import")
        return TokenKind::k_import;
      if (identifierStr == "in")
        return TokenKind::k_in;
      if (identifierStr == "is")
        return TokenKind::k_is;
      if (identifierStr == "lambda")
        return TokenKind::k_lambda;
      if (identifierStr == "nonlocal")
        return TokenKind::k_nonlocal;
      if (identifierStr == "not")
        return TokenKind::k_not;
      if (identifierStr == "or")
        return TokenKind::k_or;
      if (identifierStr == "pass")
        return TokenKind::k_pass;
      if (identifierStr == "raise")
        return TokenKind::k_raise;
      if (identifierStr == "return")
        return TokenKind::k_return;
      if (identifierStr == "try")
        return TokenKind::k_try;
      if (identifierStr == "while")
        return TokenKind::k_while;
      if (identifierStr == "with")
        return TokenKind::k_with;
      if (identifierStr == "yield")
        return TokenKind::k_yield;
      return TokenKind::kIdentifier;
    }

    // integer literal: [0-9]+
    if (isdigit(lastChar)) {
      std::string intStr;
      do {
        intStr += lastChar;
        lastChar = getNextChar();
      } while (isdigit(lastChar));
      intValue = std::stoll(intStr);
      return TokenKind::kIntegerLiteral;
    }

    if (lastChar == '#') {
      // comment until end of line.
      do {
        lastChar = getNextChar();
      } while (lastChar != EOF && lastChar != '\n');

      if (lastChar != EOF)
        return getToken();
    }

    // string literals and id literals
    if (lastChar == '"') {
      strLiteral = "";
      lastChar = getNextChar();
      while (lastChar != EOF && lastChar != '"') {
        if (lastChar == '\\') {
          lastChar = getNextChar();
          if (lastChar == 'n')
            strLiteral += '\n';
          else if (lastChar == 't')
            strLiteral += '\t';
          else if (lastChar == '"')
            strLiteral += '"';
          else if (lastChar == '\\')
            strLiteral += '\\';
          else
            return TokenKind::kUnknown;
        } else {
          if (lastChar < 32 || lastChar > 126) {
            return TokenKind::kUnknown;
          }
          strLiteral += lastChar;
        }
        lastChar = getNextChar();
      }
      lastChar = getNextChar();
      return TokenKind::kStringLiteral;
    }

    if (lastChar == '+') {
      lastChar = getNextChar();
      return TokenKind::kPlus;
    }

    if (lastChar == '-') {
      lastChar = getNextChar();
      if (lastChar == '>') {
        lastChar = getNextChar();
        return TokenKind::kArrow;
      }
      return TokenKind::kMinus;
    }

    if (lastChar == '*') {
      lastChar = getNextChar();
      return TokenKind::kMul;
    }

    if (lastChar == '/') {
      lastChar = getNextChar();
      if (lastChar == '/') {
        lastChar = getNextChar();
        return TokenKind::kIntDiv;
      }
      return TokenKind::kUnknown;
    }

    if (lastChar == '%') {
      lastChar = getNextChar();
      return TokenKind::kMod;
    }

    if (lastChar == '<') {
      lastChar = getNextChar();
      if (lastChar == '=') {
        lastChar = getNextChar();
        return TokenKind::kLessThanOrEqual;
      }
      return TokenKind::kLessThan;
    }

    if (lastChar == '>') {
      lastChar = getNextChar();
      if (lastChar == '=') {
        lastChar = getNextChar();
        return TokenKind::kGreaterThanOrEqual;
      }
      return TokenKind::kGreaterThan;
    }

    if (lastChar == '=') {
      lastChar = getNextChar();
      if (lastChar == '=') {
        lastChar = getNextChar();
        return TokenKind::kEqual;
      }
      return TokenKind::kAssign;
    }

    if (lastChar == '!') {
      lastChar = getNextChar();
      if (lastChar == '=') {
        lastChar = getNextChar();
        return TokenKind::kInEqual;
      }
      return TokenKind::kUnknown;
    }

    if (lastChar == '(') {
      lastChar = getNextChar();
      return TokenKind::kOpenParantheses;
    }

    if (lastChar == ')') {
      lastChar = getNextChar();
      return TokenKind::kCloseParantheses;
    }

    if (lastChar == '[') {
      lastChar = getNextChar();
      return TokenKind::kOpenSquareBracket;
    }

    if (lastChar == ']') {
      lastChar = getNextChar();
      return TokenKind::kCloseSquareBracket;
    }

    if (lastChar == ',') {
      lastChar = getNextChar();
      return TokenKind::kComma;
    }

    if (lastChar == ':') {
      lastChar = getNextChar();
      return TokenKind::kColon;
    }

    if (lastChar == '.') {
      lastChar = getNextChar();
      return TokenKind::kAttrAccessOp;
    }

    if (lastChar == EOF)
      return TokenKind::kEOF;

    return TokenKind::kUnknown;
  }

  struct LexerState {
    Location lastLocation;
    int curLineNum;
    int curCol;
    llvm::StringRef curLineBuffer;
    TokenKind curTok;
    TokenKind firstTokenInIndentedLogicalLine;
    std::string identifierStr;
    int64_t intValue;
    std::string strLiteral;
    char lastChar;
    bool isLogicalLine;
    int prevLineIndentLevel;
    int currentLineIndentLevel;
    bool processingLeadingSpaces;
    bool indentDenentDone;
  };

  LexerState saveState() const {
    return LexerState{lastLocation,
                      curLineNum,
                      curCol,
                      curLineBuffer,
                      curTok,
                      firstTokenInIndentedLogicalLine,
                      identifierStr,
                      intValue,
                      strLiteral,
                      lastChar,
                      isLogicalLine,
                      prevLineIndentLevel,
                      currentLineIndentLevel,
                      processingLeadingSpaces,
                      indentDenentDone};
  }

  void restoreState(const LexerState& state) {
    lastLocation = state.lastLocation;
    curLineNum = state.curLineNum;
    curCol = state.curCol;
    curLineBuffer = state.curLineBuffer;
    curTok = state.curTok;
    firstTokenInIndentedLogicalLine = state.firstTokenInIndentedLogicalLine;
    identifierStr = state.identifierStr;
    intValue = state.intValue;
    strLiteral = state.strLiteral;
    lastChar = state.lastChar;
    isLogicalLine = state.isLogicalLine;
    prevLineIndentLevel = state.prevLineIndentLevel;
    currentLineIndentLevel = state.currentLineIndentLevel;
    processingLeadingSpaces = state.processingLeadingSpaces;
    indentDenentDone = state.indentDenentDone;
  }

  Location lastLocation;
  int curLineNum = 0;
  int curCol = 0;
  llvm::StringRef curLineBuffer = "\n";
  TokenKind curTok;

  TokenKind firstTokenInIndentedLogicalLine = TokenKind::kUnknown;

  std::string identifierStr;
  int64_t intValue = 0;
  std::string strLiteral;

  char lastChar = '\0';
  // flag for processing indentation
  bool isLogicalLine = false;

  int prevLineIndentLevel = 0;
  int currentLineIndentLevel = 0;

  bool processingLeadingSpaces = true;
  bool indentDenentDone = false;
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

#endif // CHOCOPY_LEXER_H
