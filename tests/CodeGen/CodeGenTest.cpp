#include <fstream>
#include <iostream>

#include "gtest/gtest.h"

#include "AST.h"
#include "CodeGen.h"
#include "Lexer.h"
#include "Parser.h"
#include "SemanticCheck.h"

namespace chocopy {

std::string runIRAndGetOutput(const std::string& irFile) {
  std::string base = irFile;
  if (base.size() > 6 && base.substr(base.size() - 6) == ".py.ll")
    base = base.substr(0, base.size() - 6);
  std::string binaryFile = base + ".bin";
  std::string clangCmd =
      "clang-17 -x ir " + irFile + ".ll" + " -o " + binaryFile + " -g 2>&1";
  int clangResult = std::system(clangCmd.c_str());
  EXPECT_EQ(clangResult, 0) << "Clang failed to compile IR";

  std::string runCmd = binaryFile + " > " + irFile + ".out";
  int runResult = std::system(runCmd.c_str());
  EXPECT_EQ(runResult, 0) << "Binary execution failed";

  std::ifstream outFile(irFile + ".out");
  std::stringstream buffer;
  buffer << outFile.rdbuf();
  return buffer.str();
}

std::string readFileToString(const std::string& filename) {
  std::ifstream inFile(filename);
  std::stringstream buffer;
  buffer << inFile.rdbuf();
  return buffer.str();
}

TEST(CodeGenTest, MultiplePrograms) {
  std::map<std::string, std::string> expectedOutput = {
      {"00", "Hello, World!\n"},
      {"01", "42\nTrue\nFalse\n"},
      {"02", "12\n"},
      {"03", "shift using rear wheels\nshift using all wheels\n"},
      {"04", "100\nTrue\ntest\n"},
      {"05", "77\n201\n100\n"},
      {"06", "-10\n34\n706\n1080\n4\n2\n"},
      {"07", "False\nFalse\nTrue\nFalse\n"},
      {"08", "10\nDriving SUV\n"},
      {"09", "23\n0\nInner If\n5\n"},
      {"10", "True\nFalse\n40\n60\n100\n150\n"},
      {"11", "Buzz\n19\nFizz\n17\n16\nFizzBuzz\n14\n13\nFizz\n11\nBuzz\nFizz\n8"
             "\n7\nFizz\nBuzz\n4\nFizz\n2\n1\nRESULT:\n10\n"},
      {"12", "20\n5\n45\n10\nFalse\nTrue\n"}};

  for (int i = 0; i < expectedOutput.size(); ++i) {
    std::ostringstream oss;
    oss << std::setw(2) << std::setfill('0') << i;
    std::string idx = oss.str();
    std::string filename = "./testprograms/chocopy_" + idx + ".py";

    SCOPED_TRACE("Testing file: " + filename);

    std::string program = readFileToString(filename);

    LexerBuffer lexer(program.c_str(), program.c_str() + program.size(),
                      filename.c_str());
    chocopy::Parser parser(lexer);
    std::unique_ptr<chocopy::ProgramAST> programAST = parser.parseProgram();
    ASSERT_NE(programAST, nullptr);

    chocopy::SemanticCheckVisitor semanticCheck;
    auto errors = semanticCheck.check(*programAST);
    for (const auto& err : errors) {
      std::cerr << filename << err.getErrorMsg();
    }
    ASSERT_TRUE(errors.empty());

    chocopy::LLVMCodeGenVisitor codeGenVisitor(programAST.get(), filename);
    codeGenVisitor.codeGen();
    codeGenVisitor.printLLVMBitCode(filename);

    std::string output = runIRAndGetOutput(filename);
    EXPECT_EQ(output, expectedOutput[idx]);

    std::string llFile = filename + ".ll";
    std::string binFile = filename + ".bin";
    std::string outFile = filename + ".out";
    std::remove(llFile.c_str());
    std::remove(binFile.c_str());
    std::remove(outFile.c_str());
  }
}

} // namespace chocopy
