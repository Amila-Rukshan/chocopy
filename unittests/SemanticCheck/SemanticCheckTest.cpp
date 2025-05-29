#include "gtest/gtest.h"

#include "AST.h"
#include "Lexer.h"
#include "Parser.h"
#include "SemanticCheck.h"

namespace chocopy {

TEST(SemanticCheckTest, TestSimplestClassDef) {

  std::string program = R"(
class vehicle(object):
    pass
)";
  LexerBuffer lexer(program.c_str(), program.c_str() + program.size(),
                    "test.py");
  chocopy::Parser parser(lexer);
  std::unique_ptr<chocopy::ProgramAST> programAST = parser.parseProgram();
  ASSERT_NE(programAST, nullptr);

  chocopy::SemanticCheckVisitor semanticCheck;
  auto errors = semanticCheck.check(*programAST);

  ASSERT_TRUE(errors.empty());
}

TEST(SemanticCheckTest, TestInheritFromUndefinedType) {

  std::string program = R"(
class vehicle(magic):
    pass
)";
  LexerBuffer lexer(program.c_str(), program.c_str() + program.size(),
                    "test.py");
  chocopy::Parser parser(lexer);
  std::unique_ptr<chocopy::ProgramAST> programAST = parser.parseProgram();
  ASSERT_NE(programAST, nullptr);

  chocopy::SemanticCheckVisitor semanticCheck;
  auto errors = semanticCheck.check(*programAST);

  ASSERT_FALSE(errors.empty());
  ASSERT_EQ(errors.size(), 1);
  ASSERT_EQ(errors[0].getErrorMsg(),
            ":2:15: Inherited class 'magic' is not found\n");
}

TEST(SemanticCheckTest, TestInheritFromPrimitiveTypes) {

  std::string program = R"(
class shoe(str):
    pass
class bag(bool):
    pass
class watch(int):
    pass
)";
  LexerBuffer lexer(program.c_str(), program.c_str() + program.size(),
                    "test.py");
  chocopy::Parser parser(lexer);
  std::unique_ptr<chocopy::ProgramAST> programAST = parser.parseProgram();
  ASSERT_NE(programAST, nullptr);

  chocopy::SemanticCheckVisitor semanticCheck;
  auto errors = semanticCheck.check(*programAST);

  ASSERT_FALSE(errors.empty());
  ASSERT_EQ(errors.size(), 3);
  ASSERT_EQ(errors[0].getErrorMsg(),
            ":2:12: Cannot inherit from built-in type 'str'\n");
  ASSERT_EQ(errors[1].getErrorMsg(),
            ":4:11: Cannot inherit from built-in type 'bool'\n");
  ASSERT_EQ(errors[2].getErrorMsg(),
            ":6:13: Cannot inherit from built-in type 'int'\n");
}

TEST(SemanticCheckTest, TestInheritFromItself) {

  std::string program = R"(
class vehicle(vehicle):
    pass
)";
  LexerBuffer lexer(program.c_str(), program.c_str() + program.size(),
                    "test.py");
  chocopy::Parser parser(lexer);
  std::unique_ptr<chocopy::ProgramAST> programAST = parser.parseProgram();
  ASSERT_NE(programAST, nullptr);

  chocopy::SemanticCheckVisitor semanticCheck;
  auto errors = semanticCheck.check(*programAST);

  ASSERT_FALSE(errors.empty());
  ASSERT_EQ(errors.size(), 2);
  ASSERT_EQ(errors[0].getErrorMsg(),
            ":2:15: class 'vehicle' cannot inherit from itself\n");
  ASSERT_EQ(errors[1].getErrorMsg(),
            ":2:15: Inherited class 'vehicle' is not found\n");
}

TEST(SemanticCheckTest, TestMethodWithNoSelfArg) {
  std::string program = R"(
class vehicle(object):
    def drive():
        pass
)";
  LexerBuffer lexer(program.c_str(), program.c_str() + program.size(),
                    "test.py");
  chocopy::Parser parser(lexer);
  std::unique_ptr<chocopy::ProgramAST> programAST = parser.parseProgram();
  ASSERT_NE(programAST, nullptr);

  chocopy::SemanticCheckVisitor semanticCheck;
  auto errors = semanticCheck.check(*programAST);

  ASSERT_FALSE(errors.empty());
  ASSERT_EQ(errors.size(), 1);
  ASSERT_EQ(errors[0].getErrorMsg(),
            ":3:9: Method 'drive' in class 'vehicle' must have at least one "
            "parameter 'self'\n");
}

TEST(SemanticCheckTest, TestMethodWithSelfParamWithWrongName) {
  std::string program = R"(
class vehicle(object):
    def drive(notSelf: "vehicle"):
        pass
)";
  LexerBuffer lexer(program.c_str(), program.c_str() + program.size(),
                    "test.py");
  chocopy::Parser parser(lexer);
  std::unique_ptr<chocopy::ProgramAST> programAST = parser.parseProgram();
  ASSERT_NE(programAST, nullptr);

  chocopy::SemanticCheckVisitor semanticCheck;
  auto errors = semanticCheck.check(*programAST);

  ASSERT_FALSE(errors.empty());
  ASSERT_EQ(errors.size(), 1);
  ASSERT_EQ(errors[0].getErrorMsg(),
            ":3:15: First parameter of method 'drive' in class 'vehicle' must "
            "be named 'self'\n");
}

TEST(SemanticCheckTest, TestMethodWithSelfParamWithWrongType) {
  std::string program = R"(
class vehicle(object):
    def drive(self: "object"):
        pass
)";
  LexerBuffer lexer(program.c_str(), program.c_str() + program.size(),
                    "test.py");
  chocopy::Parser parser(lexer);
  std::unique_ptr<chocopy::ProgramAST> programAST = parser.parseProgram();
  ASSERT_NE(programAST, nullptr);

  chocopy::SemanticCheckVisitor semanticCheck;
  auto errors = semanticCheck.check(*programAST);

  ASSERT_FALSE(errors.empty());
  ASSERT_EQ(errors.size(), 1);
  ASSERT_EQ(errors[0].getErrorMsg(),
            ":3:15: First parameter 'self' of method 'drive' must be of type "
            "of its class 'vehicle'\n");
}

} // namespace chocopy
