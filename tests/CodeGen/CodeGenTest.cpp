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
  std::string clangCmd = "clang-17 " + irFile + ".ll";
#ifdef CHOCOPY_LIB_PATH
  clangCmd += " " + std::string(CHOCOPY_LIB_PATH);
#endif
  clangCmd += " -o " + binaryFile + " -g 2>&1";
  int clangResult = std::system(clangCmd.c_str());
  EXPECT_EQ(clangResult, 0) << "Clang failed to compile IR";

  std::string runCmd = binaryFile + " > " + irFile + ".out";
  int runResult = std::system(runCmd.c_str());

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
      {"12", "20\n5\n45\n10\nFalse\nTrue\n"},
      {"13", "Rust\nC++\nMojo\n"},
      {"14", "False\nTrue\nTrue\nFalse\nTrue\nFalse\n"},
      {"15", "x\n4\ny\n1\nz\n0\ny\n0\nx\n2\ny\n0\nx\n1\ny\n0\nx\n0\n===== "
             "fibonacci numbers =====\n0\n1\n1\n2\n3\n5\n8\n13\n21\n34\n"},
      {"16", "FirstSecond\n"},
      {"17", "True\nA\nE\nI\nO\nU\n"},
      {"18", "12\nAa\n79\nAaaA\n101\nAadD\n"},
      {"19", "100\n1\n23\n11\n4\n200\n6\n7\n8\n9\n300\n"},
      {"20", "6\n8\nTrue\n8\n6\nTrue\n"},
      {"21", "4\n3\n0\n4\n-6\n0\n"},
      {"22",
       "12\n45\n23\nthis\nis\ncool\nTrue\nFalse\nTrue\nc\nh\no\nc\no\np\ny\n"},
      {"23", "True\nFalse\n"},
      {"24", "test A\ntest B\ntest B\n"},
      {"25", "AA\nAA\nAA\nAA\n"},
      {"26", "Item found!\nItem found!\n"},
      {"27", "2\n21\n23\n1\nTrue\nFalse\n2\nFoo\nBar\nT1\n6\nT2\n7\n"},
      {"28", "./testprograms/chocopy_28.py:7:7: Runtime error: List index out "
             "of bounds\n"},
      {"29", "new\n4\n2\n0\n9\n4\nQQqQQ\nPPpPP\n"},
      {"30", "4\n77\n77\n77\n"},
      {"31", "1\n2\n4\n4\n8\n8\n8\n8\n16\n16\n16\n16\n16\n16\n16\n16\n17\n18\n1"
             "9\n20\n"},
      {"32", "foo called\ninner\nbar-inner\ndefault\nouter\n"},
      {"33", "45\n"},
      {"34", "12\n5\nFalse\n17\n"},
      {"35", "1024\n27\n2\n"},
      {"36", "500\n"}};

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
