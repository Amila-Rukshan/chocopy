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

TEST(LexerTest, TestIgnoringCommentsAndNonLogicalLines) {
  std::string program = R"(
 # comment
def bar():
    # comment
    if x:#commet
        return True  #commet
    
 # comment
  
 
    else:
        return False
)";
  LexerBuffer lexer(program.c_str(), program.c_str() + program.size(),
                    "test.py");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::k_def);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "bar");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kOpenParantheses);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kCloseParantheses);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kColon);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kNewLine);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIndent);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::k_if);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "x");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kColon);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kNewLine);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIndent);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::k_return);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::k_True);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kNewLine);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kDedent);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::k_else);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kColon);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kNewLine);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIndent);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::k_return);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::k_False);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kNewLine);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kDedent);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kDedent);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kEOF);
}

TEST(LexerTest, TestClasses) {
  std::string program = R"(
class animal(object):
    makes_noise:bool = False

    def make_noise(self: "animal") -> object:
        if (self.makes_noise):
            print(self.sound())

    def sound(self: "animal") -> str:
        return "???"

class cow(animal):
    def __init__(self: "cow"):
        self.makes_noise = True

    def sound(self: "cow") -> str:
        return "moo"
 # test comment

c:animal = None
c = cow()
c.make_noise()

)";

  LexerBuffer lexer(program.c_str(), program.c_str() + program.size(),
                    "test.py");

  EXPECT_EQ(lexer.getNextToken(), TokenKind::k_class);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "animal");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kOpenParantheses);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "object");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kCloseParantheses);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kColon);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kNewLine);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIndent);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "makes_noise");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kColon);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "bool");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kAssign);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::k_False);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kNewLine);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::k_def);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "make_noise");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kOpenParantheses);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "self");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kColon);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kStringLiteral);
  EXPECT_EQ(lexer.getStringLiteral(), "animal");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kCloseParantheses);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kArrow);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "object");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kColon);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kNewLine);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIndent);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::k_if);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kOpenParantheses);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "self");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kAttrAccessOp);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "makes_noise");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kCloseParantheses);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kColon);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kNewLine);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIndent);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "print");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kOpenParantheses);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "self");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kAttrAccessOp);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "sound");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kOpenParantheses);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kCloseParantheses);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kCloseParantheses);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kNewLine);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kDedent);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kDedent);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::k_def);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "sound");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kOpenParantheses);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "self");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kColon);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kStringLiteral);
  EXPECT_EQ(lexer.getStringLiteral(), "animal");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kCloseParantheses);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kArrow);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "str");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kColon);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kNewLine);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIndent);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::k_return);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kStringLiteral);
  EXPECT_EQ(lexer.getStringLiteral(), "???");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kNewLine);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kDedent);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kDedent);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::k_class);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "cow");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kOpenParantheses);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "animal");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kCloseParantheses);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kColon);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kNewLine);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIndent);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::k_def);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "__init__");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kOpenParantheses);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "self");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kColon);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kStringLiteral);
  EXPECT_EQ(lexer.getStringLiteral(), "cow");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kCloseParantheses);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kColon);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kNewLine);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIndent);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "self");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kAttrAccessOp);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "makes_noise");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kAssign);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::k_True);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kNewLine);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kDedent);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::k_def);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "sound");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kOpenParantheses);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "self");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kColon);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kStringLiteral);
  EXPECT_EQ(lexer.getStringLiteral(), "cow");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kCloseParantheses);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kArrow);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "str");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kColon);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kNewLine);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIndent);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::k_return);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kStringLiteral);
  EXPECT_EQ(lexer.getStringLiteral(), "moo");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kNewLine);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kDedent);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kDedent);

  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "c");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kColon);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "animal");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kAssign);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::k_None);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kNewLine);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "c");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kAssign);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "cow");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kOpenParantheses);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kCloseParantheses);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kNewLine);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "c");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kAttrAccessOp);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kIdentifier);
  EXPECT_EQ(lexer.getIdentifier(), "make_noise");
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kOpenParantheses);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kCloseParantheses);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kNewLine);
  EXPECT_EQ(lexer.getNextToken(), TokenKind::kEOF);
}

} // namespace chocopy
