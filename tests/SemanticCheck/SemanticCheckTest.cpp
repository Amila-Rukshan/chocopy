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

TEST(SemanticCheckTest, TestIntegerArithmetic) {
  std::string program = R"(
i:int = 1
b: bool = True
k: int = 0

print(i + b)
print(i - False)
print("str" * i)
print(i // True)
print(i % "devider")
)";

  LexerBuffer lexer(program.c_str(), program.c_str() + program.size(),
                    "test.py");
  chocopy::Parser parser(lexer);
  std::unique_ptr<chocopy::ProgramAST> programAST = parser.parseProgram();
  ASSERT_NE(programAST, nullptr);

  chocopy::SemanticCheckVisitor semanticCheck;
  auto errors = semanticCheck.check(*programAST);

  ASSERT_FALSE(errors.empty());
  ASSERT_EQ(errors.size(), 5);
  ASSERT_EQ(errors[0].getErrorMsg(),
            ":6:9: Unsupported types for '+' operator: 'int' and 'bool'\n");
  ASSERT_EQ(errors[1].getErrorMsg(),
            ":7:9: Unsupported types for '-' operator: 'int' and 'bool'\n");
  ASSERT_EQ(errors[2].getErrorMsg(),
            ":8:13: Unsupported types for '*' operator: 'str' and 'int'\n");
  ASSERT_EQ(errors[3].getErrorMsg(),
            ":9:9: Unsupported types for '//' operator: 'int' and 'bool'\n");
  ASSERT_EQ(errors[4].getErrorMsg(),
            ":10:9: Unsupported types for '%' operator: 'int' and 'str'\n");
}

TEST(SemanticCheckTest, TestDivisionByZero) {
  std::string program = R"(
i:int = 1
k: int = 0
k = i // 0

k = i % 0
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
            ":4:7: Division by zero in '//' operator\n");
  ASSERT_EQ(errors[1].getErrorMsg(),
            ":6:7: Division by zero in '%' operator\n");
}

TEST(SemanticCheckTest, TestIfElseExpression) {
  std::string program = R"(
a: bool = True
z: int = 0
z = 1 if a else 2
z = 1 if a else "str"
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
            ":5:7: If and else bodies must have the same primitive type, found "
            "'int' and 'str'\n");
  ASSERT_EQ(errors[1].getErrorMsg(),
            ":5:3: Type mismatch: cannot assign 'object' to 'int'\n");
}

TEST(SemanticCheckTest, TestStrLenAndStrIndexUsage) {
  std::string program = R"(
s: str = "hello"
b: bool = False
i: int = 0

s = s + s[0]
i = i + len(s)
b = b or len(s)

print(s)
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
            ":8:7: Unsupported types for 'k_or' operator: 'bool' and 'int'\n");
  ASSERT_EQ(errors[1].getErrorMsg(),
            ":8:3: Type mismatch: cannot assign 'UNTYPED' to 'bool'\n");
}

TEST(SemanticCheckTest, TestArgTypesAndReturnTypeMismatch) {
  std::string program = R"(# A broken program
def is_even(x:int) -> bool:
    if x % 2 == 1:
        return 0      # FIXME
    else:
        return True

print(is_even("3"))   # FIXME

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
            ":4:9: Return type mismatch: expected 'bool', found 'int'\n");
  ASSERT_EQ(errors[1].getErrorMsg(), ":8:7: Argument 1 of function 'is_even' "
                                     "must be of type 'int', but got 'str'\n");
}

TEST(SemanticCheckTest, TestListType) {
  std::string program = R"(
arr: [[int]] = None
arr = [[1, 2, 3], [4, 5, 6]]

arr = [1, 2]
arr = [["a", "b"], ["c"]]
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
            ":5:5: Type mismatch: cannot assign 'int[]' to 'int[[]]'\n");
  ASSERT_EQ(errors[1].getErrorMsg(),
            ":6:5: Type mismatch: cannot assign 'str[[]]' to 'int[[]]'\n");
}

TEST(SemanticCheckTest, TestNoneAndListAssignability) {
  std::string program = R"(
i: int = 0
a: [int] = None
b: [bool] = None
i = None
a = b
a = [None]
a = []
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
            ":5:3: Type mismatch: cannot assign '<None>' to 'int'\n");
  ASSERT_EQ(errors[1].getErrorMsg(),
            ":6:3: Type mismatch: cannot assign 'bool[]' to 'int[]'\n");
  ASSERT_EQ(errors[2].getErrorMsg(),
            ":7:3: Type mismatch: cannot assign '<None>[]' to 'int[]'\n");
}

TEST(SemanticCheckTest, TestUnknownIdentifiersAndFunctions) {
  std::string program = R"(
print(undefined)
foo(1)
print(len("a", "b"))
print(len)
)";

  LexerBuffer lexer(program.c_str(), program.c_str() + program.size(),
                    "test.py");
  chocopy::Parser parser(lexer);
  std::unique_ptr<chocopy::ProgramAST> programAST = parser.parseProgram();
  ASSERT_NE(programAST, nullptr);

  chocopy::SemanticCheckVisitor semanticCheck;
  auto errors = semanticCheck.check(*programAST);

  ASSERT_FALSE(errors.empty());
  ASSERT_EQ(errors.size(), 4);
  ASSERT_EQ(errors[0].getErrorMsg(), ":2:16: Unknown identifier: undefined\n");
  ASSERT_EQ(errors[1].getErrorMsg(), ":3:1: Unknown function: foo\n");
  ASSERT_EQ(errors[2].getErrorMsg(),
            ":4:7: Function 'len' expects 1 arguments, but got 2\n");
  ASSERT_EQ(errors[3].getErrorMsg(), ":5:10: Not a variable: len\n");
}

TEST(SemanticCheckTest, TestAttributeRedefinitionAndMethodOverride) {
  std::string program = R"(
class A(object):
    x: int = 0
    def foo(self: "A", y: int) -> int:
        return y

class B(A):
    x: int = 1
    def foo(self: "B", y: str) -> int:
        return 0

class C(object):
    def __init__(self: "C", v: int):
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
  ASSERT_EQ(errors[0].getErrorMsg(), ":8:6: Cannot re-define attribute: x\n");
  ASSERT_EQ(errors[1].getErrorMsg(),
            ":9:9: Method overridden with different type signature: foo\n");
  ASSERT_EQ(
      errors[2].getErrorMsg(),
      ":13:9: Method overridden with different type signature: __init__\n");
}

TEST(SemanticCheckTest, TestConstructorAndInheritedMethodDispatch) {
  std::string program = R"(
class A(object):
    def foo(self: "A", y: int) -> int:
        return y

class B(A):
    pass

b: B = None
b = B(1)
b.foo("x")
b.bar()
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
            ":10:5: Function 'B' expects 0 arguments, but got 1\n");
  ASSERT_EQ(errors[1].getErrorMsg(), ":11:3: Argument 1 of function 'foo' must "
                                     "be of type 'int', but got 'str'\n");
  ASSERT_EQ(errors[2].getErrorMsg(), ":12:3: Unknown method: bar\n");
}

TEST(SemanticCheckTest, TestMissingReturnAndTopLevelReturn) {
  std::string program = R"(
def f(x: int) -> int:
    if x > 0:
        return 1

def g(x: int) -> int:
    if x > 0:
        return 1
    else:
        return 0

return 1
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
  ASSERT_EQ(errors[0].getErrorMsg(), ":2:5: All paths in this function/method "
                                     "must have a return statement: f\n");
  ASSERT_EQ(errors[1].getErrorMsg(),
            ":12:1: Return statement cannot appear at the top level\n");
}

TEST(SemanticCheckTest, TestScopeRules) {
  std::string program = R"(
x: int = 0
x: int = 1

def h(int: bool):
    global y
    x = 5

def outer():
    v: int = 0
    def inner():
        nonlocal v
        nonlocal w
        v = 1
    v = 2
)";

  LexerBuffer lexer(program.c_str(), program.c_str() + program.size(),
                    "test.py");
  chocopy::Parser parser(lexer);
  std::unique_ptr<chocopy::ProgramAST> programAST = parser.parseProgram();
  ASSERT_NE(programAST, nullptr);

  chocopy::SemanticCheckVisitor semanticCheck;
  auto errors = semanticCheck.check(*programAST);

  ASSERT_FALSE(errors.empty());
  ASSERT_EQ(errors.size(), 5);
  ASSERT_EQ(errors[0].getErrorMsg(),
            ":3:2: Duplicate declaration of identifier in same scope: x\n");
  ASSERT_EQ(errors[1].getErrorMsg(), ":5:7: Cannot shadow class name: int\n");
  ASSERT_EQ(errors[2].getErrorMsg(), ":5:5: Not a global variable: y\n");
  ASSERT_EQ(errors[3].getErrorMsg(),
            ":7:7: Cannot assign to variable that is not explicitly declared "
            "in this scope: x\n");
  ASSERT_EQ(errors[4].getErrorMsg(), ":11:9: Not a nonlocal variable: w\n");
}

TEST(SemanticCheckTest, TestStrIndexMultiAssignAndForTarget) {
  std::string program = R"(
s: str = "abc"
xs: [int] = None
ys: [int] = None
c: str = ""
s[0] = "x"
xs = ys = [None]
for c in [1, 2]:
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
  ASSERT_EQ(errors.size(), 5);
  ASSERT_EQ(errors[0].getErrorMsg(),
            ":6:2: Cannot assign to an index of a 'str' value\n");
  ASSERT_EQ(
      errors[1].getErrorMsg(),
      ":7:4: Right-hand side of multiple assignment may not be [<None>]\n");
  ASSERT_EQ(errors[2].getErrorMsg(),
            ":7:4: Type mismatch: cannot assign '<None>[]' to 'int[]'\n");
  ASSERT_EQ(errors[3].getErrorMsg(),
            ":7:9: Type mismatch: cannot assign '<None>[]' to 'int[]'\n");
  ASSERT_EQ(errors[4].getErrorMsg(),
            ":8:5: Type mismatch: cannot assign 'int' to 'str'\n");
}

TEST(SemanticCheckTest, TestAttributeAndLocalVarInit) {
  std::string program = R"(
class A(object):
    x: int = "one"

def f():
    y: str = 1
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
  ASSERT_EQ(errors[0].getErrorMsg(), ":3:14: Variable 'x' of type 'int' must "
                                     "be initialized with a number literal\n");
  ASSERT_EQ(errors[1].getErrorMsg(), ":6:14: Variable 'y' of type 'str' must "
                                     "be initialized with a string literal\n");
}

TEST(SemanticCheckTest, TestObjectOnlyAsSuperclass) {
  std::string program = R"(
class A(object):
    x: object = None
    def foo(self: "A", y: object) -> object:
        return None

o: object = None
l: [object] = None

def f() -> "object":
    def g(z: int):
        pass
    return None
)";

  LexerBuffer lexer(program.c_str(), program.c_str() + program.size(),
                    "test.py");
  chocopy::Parser parser(lexer);
  std::unique_ptr<chocopy::ProgramAST> programAST = parser.parseProgram();
  ASSERT_NE(programAST, nullptr);

  chocopy::SemanticCheckVisitor semanticCheck;
  auto errors = semanticCheck.check(*programAST);

  ASSERT_FALSE(errors.empty());
  ASSERT_EQ(errors.size(), 6);
  ASSERT_EQ(errors[0].getErrorMsg(),
            ":7:4: Type 'object' can only be used as a superclass\n");
  ASSERT_EQ(errors[1].getErrorMsg(),
            ":8:5: Type 'object' can only be used as a superclass\n");
  ASSERT_EQ(errors[2].getErrorMsg(),
            ":3:8: Type 'object' can only be used as a superclass\n");
  ASSERT_EQ(errors[3].getErrorMsg(),
            ":4:27: Type 'object' can only be used as a superclass\n");
  ASSERT_EQ(errors[4].getErrorMsg(),
            ":4:38: Type 'object' can only be used as a superclass\n");
  ASSERT_EQ(errors[5].getErrorMsg(),
            ":10:12: Type 'object' can only be used as a superclass\n");
}

} // namespace chocopy
