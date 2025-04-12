#include "Lexer.h"
#include "gtest/gtest.h"

namespace chocopy {

// Test case for tokenizing a non-nested function and calling it
TEST(LexerTest, NonNestedFunction) {
  std::string program = R"(
def foo():
    x: int = 42
    print(x)

foo()
)";
  LexerBuffer lexer(program.c_str(), program.c_str() + program.size(),
                    "test.py");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::k_def);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "foo");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kOpenParantheses);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kCloseParantheses);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kColon);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kNewLine);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIndent);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "x");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kColon);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "int");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kAssign);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIntegerLiteral);
  EXPECT_EQ(lexer.getIntegerValue(), 42);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kNewLine);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "print");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kOpenParantheses);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "x");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kCloseParantheses);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kNewLine);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kDedent);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "foo");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kOpenParantheses);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kCloseParantheses);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kNewLine);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kEOF);
}

// Test case for tokenizing a nested function
TEST(LexerTest, NestedFunction) {
  std::string program = R"(

def foo(x:int) -> bool:
    a:int = 0
    b:int = 1
    def bar(y: int) -> int:
        a:int = 2
        return y
    return bar(x) > a

#comment
foo(1)
)";
  LexerBuffer lexer(program.c_str(), program.c_str() + program.size(),
                    "test.py");

  EXPECT_EQ(lexer.getNextToken(), TokenKind::k_def);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "foo");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kOpenParantheses);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "x");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kColon);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "int");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kCloseParantheses);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kArrow);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "bool");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kColon);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kNewLine);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIndent);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "a");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kColon);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "int");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kAssign);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIntegerLiteral);
  EXPECT_EQ(lexer.getIntegerValue(), 0);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kNewLine);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "b");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kColon);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "int");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kAssign);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIntegerLiteral);
  EXPECT_EQ(lexer.getIntegerValue(), 1);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kNewLine);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::k_def);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "bar");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kOpenParantheses);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "y");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kColon);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "int");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kCloseParantheses);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kArrow);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "int");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kColon);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kNewLine);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIndent);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "a");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kColon);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "int");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kAssign);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIntegerLiteral);
  EXPECT_EQ(lexer.getIntegerValue(), 2);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kNewLine);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::k_return);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "y");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kNewLine);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kDedent);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::k_return);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "bar");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kOpenParantheses);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "x");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kCloseParantheses);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kGreaterThan);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "a");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kNewLine);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kDedent);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "foo");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kOpenParantheses);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIntegerLiteral);
  EXPECT_EQ(lexer.getIntegerValue(), 1);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kCloseParantheses);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kNewLine);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kEOF);
}

} // namespace chocopy
