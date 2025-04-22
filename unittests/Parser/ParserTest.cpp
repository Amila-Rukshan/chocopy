#include "Parser.h"
#include "Lexer.h"

#include "gtest/gtest.h"

namespace chocopy {

// Test top level var declarations
TEST(ParserTest, TestGlobalVarDeclarations) {
  std::string program = R"(
x: int = 10
y: "Cow" = None
zz: [[int]] = None
qw: [[[bool]]] = "hello" # this can be parsed, but semantically not sound
)";
  LexerBuffer lexer(program.c_str(), program.c_str() + program.size(),
                    "test.py");
  Parser parser(lexer);
  auto programAST = parser.parseProgram();

  ASSERT_NE(programAST, nullptr);
  const auto& varDefs = programAST->getVarDefs();
  ASSERT_EQ(varDefs.size(), 4);

  EXPECT_EQ(varDefs[0]->getTypedVar()->getId(), "x");
  EXPECT_EQ(varDefs[0]->getTypedVar()->getType()->getKind(), TypeAST::Type_Id);
  // expect that the type is an IdTypeAST
  auto idTypeAST =
      llvm::dyn_cast<IdTypeAST>(varDefs[0]->getTypedVar()->getType());
  ASSERT_NE(idTypeAST, nullptr);
  EXPECT_EQ(idTypeAST->getId(), "int");
  // check Number literal
  auto numberLiteralAST =
      llvm::dyn_cast<LiteralNumberAST>(varDefs[0]->getLiteral());
  ASSERT_NE(numberLiteralAST, nullptr);
  EXPECT_EQ(numberLiteralAST->getNumber(), 10);

  EXPECT_EQ(varDefs[1]->getTypedVar()->getId(), "y");
  EXPECT_EQ(varDefs[1]->getTypedVar()->getType()->getKind(),
            TypeAST::Type_IdString);
  // expect that the type is an IdStringTypeAST
  auto idStringTypeAST =
      llvm::dyn_cast<IdStringTypeAST>(varDefs[1]->getTypedVar()->getType());
  ASSERT_NE(idStringTypeAST, nullptr);
  EXPECT_EQ(idStringTypeAST->getId(), "Cow");
  // check None literal
  auto noneLiteralAST =
      llvm::dyn_cast<LiteralNoneAST>(varDefs[1]->getLiteral());
  ASSERT_NE(noneLiteralAST, nullptr);

  EXPECT_EQ(varDefs[2]->getTypedVar()->getId(), "zz");
  EXPECT_EQ(varDefs[2]->getTypedVar()->getType()->getKind(),
            TypeAST::Type_List);
  // expect that the type is a ListTypeAST
  auto listTypeAST =
      llvm::dyn_cast<ListTypeAST>(varDefs[2]->getTypedVar()->getType());
  ASSERT_NE(listTypeAST, nullptr);
  EXPECT_EQ(listTypeAST->getDimension(), 2);
  EXPECT_EQ(listTypeAST->getType()->getKind(), TypeAST::Type_Id);
  // expect that the type is an IdTypeAST
  auto innerIdTypeAST = llvm::dyn_cast<IdTypeAST>(listTypeAST->getType());
  ASSERT_NE(innerIdTypeAST, nullptr);
  EXPECT_EQ(innerIdTypeAST->getId(), "int");

  EXPECT_EQ(varDefs[3]->getTypedVar()->getId(), "qw");
  EXPECT_EQ(varDefs[3]->getTypedVar()->getType()->getKind(),
            TypeAST::Type_List);
  // expect that the type is a ListTypeAST
  auto listTypeAST2 =
      llvm::dyn_cast<ListTypeAST>(varDefs[3]->getTypedVar()->getType());
  ASSERT_NE(listTypeAST2, nullptr);
  EXPECT_EQ(listTypeAST2->getDimension(), 3);
  // check String literal
  auto strLiteralAST =
      llvm::dyn_cast<LiteralStringAST>(varDefs[3]->getLiteral());
  ASSERT_NE(strLiteralAST, nullptr);
  EXPECT_EQ(strLiteralAST->getStr(), "hello");
}

} // namespace chocopy
