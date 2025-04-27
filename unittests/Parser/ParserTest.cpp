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

// Test top level simple statements
TEST(ParserTest, TestGlobalSimpleStatements) {
  std::string program = R"(
return
pass
)";
  LexerBuffer lexer(program.c_str(), program.c_str() + program.size(),
                    "test.py");
  Parser parser(lexer);
  auto programAST = parser.parseProgram();

  ASSERT_NE(programAST, nullptr);
  const auto& simpleStmts = programAST->getStmts();
  ASSERT_EQ(simpleStmts.size(), 2);

  // check that the first statement is a return statement
  auto simpleStmt1 = llvm::dyn_cast<SimpleStmtAST>(simpleStmts[0].get());
  ASSERT_NE(simpleStmt1, nullptr);
  auto simpleStmtReturn = llvm::dyn_cast<SimpleStmtReturnAST>(simpleStmt1);
  ASSERT_NE(simpleStmtReturn, nullptr);
  EXPECT_EQ(simpleStmtReturn->getExpr(), nullptr);

  // check that the second statement is a pass statement
  auto simpleStmt2 = llvm::dyn_cast<SimpleStmtAST>(simpleStmts[1].get());
  ASSERT_NE(simpleStmt2, nullptr);
  auto simpleStmtPass = llvm::dyn_cast<SimpleStmtPassAST>(simpleStmt2);
  ASSERT_NE(simpleStmtPass, nullptr);
}

// Test list literal
TEST(ParserTest, ListLiteralTest) {
  std::string program = R"(
[a, b, 1, 2]
)";

  LexerBuffer lexer(program.c_str(), program.c_str() + program.size(),
                    "test.py");

  Parser parser(lexer);

  auto programAST = parser.parseProgram();
  ASSERT_NE(programAST, nullptr);
  const auto& simpleStmts = programAST->getStmts();
  ASSERT_EQ(simpleStmts.size(), 1);

  // check that the first statement is a list literal
  auto simpleStmt1 = llvm::dyn_cast<SimpleStmtAST>(simpleStmts[0].get());
  ASSERT_NE(simpleStmt1, nullptr);
  auto simpleStmt1Expr = llvm::dyn_cast<SimpleStmtExprAST>(simpleStmt1);
  ASSERT_NE(simpleStmt1Expr, nullptr);
  auto listLiteral =
      llvm::dyn_cast<ListLiteralExprAST>(simpleStmt1Expr->getExpr());
  ASSERT_NE(listLiteral, nullptr);
  const auto& elements = listLiteral->getElements();
  ASSERT_EQ(elements.size(), 4);

  // check that the first element is an IdExprAST
  auto idExpr1 = llvm::dyn_cast<IdExprAST>(elements[0].get());
  ASSERT_NE(idExpr1, nullptr);
  EXPECT_EQ(idExpr1->getId(), "a");

  // check that the third element is an integer literal
  auto literalExpr = llvm::dyn_cast<LiteralExprAST>(elements[2].get());
  ASSERT_NE(literalExpr, nullptr);
  auto literalNumber =
      llvm::dyn_cast<LiteralNumberAST>(literalExpr->getLiteral());
  ASSERT_NE(literalNumber, nullptr);
  EXPECT_EQ(literalNumber->getNumber(), 1);
}

TEST(ParserTest, TestIndexAccessAndAttrAccess) {
  std::string program = R"(
z[w][b].a
)";

  LexerBuffer lexer(program.c_str(), program.c_str() + program.size(),
                    "test.py");

  Parser parser(lexer);

  auto programAST = parser.parseProgram();
  ASSERT_NE(programAST, nullptr);
  const auto& simpleStmts = programAST->getStmts();
  ASSERT_EQ(simpleStmts.size(), 1);

  // check that the first statement is a list literal
  auto simpleStmt1 = llvm::dyn_cast<SimpleStmtAST>(simpleStmts[0].get());
  ASSERT_NE(simpleStmt1, nullptr);
  auto simpleStmt1Expr = llvm::dyn_cast<SimpleStmtExprAST>(simpleStmt1);
  ASSERT_NE(simpleStmt1Expr, nullptr);
  auto attrAccBinaryOp =
      llvm::dyn_cast<BinaryExprAST>(simpleStmt1Expr->getExpr());
  ASSERT_NE(attrAccBinaryOp, nullptr);
  EXPECT_EQ(attrAccBinaryOp->getOp(), TokenKind::kAttrAccessOp);
  auto rhs = llvm::dyn_cast<IdExprAST>(attrAccBinaryOp->getRhs());
  ASSERT_NE(rhs, nullptr);
  EXPECT_EQ(rhs->getId(), "a");

  auto lhs = llvm::dyn_cast<BinaryExprAST>(attrAccBinaryOp->getLhs());
  ASSERT_NE(lhs, nullptr);
  EXPECT_EQ(lhs->getOp(), TokenKind::kIndexAccessOp);

  auto lhsRhs = llvm::dyn_cast<IdExprAST>(lhs->getRhs());
  ASSERT_NE(lhsRhs, nullptr);
  EXPECT_EQ(lhsRhs->getId(), "b");

  auto lhsLhs = llvm::dyn_cast<BinaryExprAST>(lhs->getLhs());
  ASSERT_NE(lhsLhs, nullptr);
  EXPECT_EQ(lhsLhs->getOp(), TokenKind::kIndexAccessOp);
  auto lhsLhsLhs = llvm::dyn_cast<IdExprAST>(lhsLhs->getLhs());
  ASSERT_NE(lhsLhsLhs, nullptr);
  EXPECT_EQ(lhsLhsLhs->getId(), "z");

  auto lhsLhsRhs = llvm::dyn_cast<IdExprAST>(lhsLhs->getRhs());
  ASSERT_NE(lhsLhsRhs, nullptr);
  EXPECT_EQ(lhsLhsRhs->getId(), "w");
}

TEST(ParserTest, TestSingleAndMultiAssignment) {
  std::string program = R"(
x = 10
y = "ChcoPy"
x.y.z.aa = A.b.zz
a = b = c = d
zz = [1, 2, 3]
)";

  LexerBuffer lexer(program.c_str(), program.c_str() + program.size(),
                    "test.py");

  Parser parser(lexer);

  auto programAST = parser.parseProgram();
  ASSERT_NE(programAST, nullptr);
  const auto& simpleStmts = programAST->getStmts();
  ASSERT_EQ(simpleStmts.size(), 5);

  auto simpleStmt1 = llvm::dyn_cast<SimpleStmtAST>(simpleStmts[0].get());
  ASSERT_NE(simpleStmt1, nullptr);
  auto simpleStmtAssign1 = llvm::dyn_cast<SimpleStmtAssignAST>(simpleStmt1);
  ASSERT_NE(simpleStmtAssign1, nullptr);
  EXPECT_EQ(simpleStmtAssign1->getTargets().size(), 1);
  EXPECT_EQ(simpleStmtAssign1->getRhs()->getKind(), ExprAST::Expr_Literal);
  auto idExpr =
      llvm::dyn_cast<IdExprAST>(simpleStmtAssign1->getTargets()[0].get());
  ASSERT_NE(idExpr, nullptr);
  EXPECT_EQ(idExpr->getId(), "x");
  auto literalExpr =
      llvm::dyn_cast<LiteralExprAST>(simpleStmtAssign1->getRhs());
  ASSERT_NE(literalExpr, nullptr);
  auto literalNumber =
      llvm::dyn_cast<LiteralNumberAST>(literalExpr->getLiteral());
  ASSERT_NE(literalNumber, nullptr);
  EXPECT_EQ(literalNumber->getNumber(), 10);

  auto simpleStmt2 = llvm::dyn_cast<SimpleStmtAST>(simpleStmts[1].get());
  ASSERT_NE(simpleStmt2, nullptr);
  auto simpleStmtAssign2 = llvm::dyn_cast<SimpleStmtAssignAST>(simpleStmt2);
  ASSERT_NE(simpleStmtAssign2, nullptr);
  EXPECT_EQ(simpleStmtAssign2->getTargets().size(), 1);
  EXPECT_EQ(simpleStmtAssign2->getRhs()->getKind(), ExprAST::Expr_Literal);
  auto idExpr2 =
      llvm::dyn_cast<IdExprAST>(simpleStmtAssign2->getTargets()[0].get());
  ASSERT_NE(idExpr2, nullptr);
  EXPECT_EQ(idExpr2->getId(), "y");
  auto literalExpr2 =
      llvm::dyn_cast<LiteralExprAST>(simpleStmtAssign2->getRhs());
  ASSERT_NE(literalExpr2, nullptr);
  auto literalString =
      llvm::dyn_cast<LiteralStringAST>(literalExpr2->getLiteral());
  ASSERT_NE(literalString, nullptr);
  EXPECT_EQ(literalString->getStr(), "ChcoPy");

  auto simpleStmt3 = llvm::dyn_cast<SimpleStmtAST>(simpleStmts[2].get());
  ASSERT_NE(simpleStmt3, nullptr);
  auto simpleStmtAssign3 = llvm::dyn_cast<SimpleStmtAssignAST>(simpleStmt3);
  ASSERT_NE(simpleStmtAssign3, nullptr);
  EXPECT_EQ(simpleStmtAssign3->getTargets().size(), 1);
  auto targetExpr =
      llvm::dyn_cast<BinaryExprAST>(simpleStmtAssign3->getTargets()[0].get());
  ASSERT_NE(targetExpr, nullptr);
  EXPECT_EQ(targetExpr->getOp(), TokenKind::kAttrAccessOp);
  auto targetLhs = llvm::dyn_cast<BinaryExprAST>(targetExpr->getLhs());
  ASSERT_NE(targetLhs, nullptr);
  EXPECT_EQ(targetLhs->getOp(), TokenKind::kAttrAccessOp);
  auto targetLhsLhs = llvm::dyn_cast<BinaryExprAST>(targetLhs->getLhs());
  ASSERT_NE(targetLhsLhs, nullptr);
  EXPECT_EQ(targetLhsLhs->getOp(), TokenKind::kAttrAccessOp);

  auto rhsOfAssignExpr =
      llvm::dyn_cast<BinaryExprAST>(simpleStmtAssign3->getRhs());
  ASSERT_NE(rhsOfAssignExpr, nullptr);
  EXPECT_EQ(rhsOfAssignExpr->getOp(), TokenKind::kAttrAccessOp);
  auto rhsOfAssignLhs =
      llvm::dyn_cast<BinaryExprAST>(rhsOfAssignExpr->getLhs());
  ASSERT_NE(rhsOfAssignLhs, nullptr);
  EXPECT_EQ(rhsOfAssignLhs->getOp(), TokenKind::kAttrAccessOp);

  auto simpleStmt4 = llvm::dyn_cast<SimpleStmtAST>(simpleStmts[3].get());
  ASSERT_NE(simpleStmt4, nullptr);
  auto simpleStmtAssign4 = llvm::dyn_cast<SimpleStmtAssignAST>(simpleStmt4);
  ASSERT_NE(simpleStmtAssign4, nullptr);
  EXPECT_EQ(simpleStmtAssign4->getTargets().size(), 3);
  EXPECT_EQ(simpleStmtAssign4->getRhs()->getKind(), ExprAST::Expr_Id);
  auto idExpr3 =
      llvm::dyn_cast<IdExprAST>(simpleStmtAssign4->getTargets()[0].get());
  ASSERT_NE(idExpr3, nullptr);
  EXPECT_EQ(idExpr3->getId(), "a");
  auto idExpr4 =
      llvm::dyn_cast<IdExprAST>(simpleStmtAssign4->getTargets()[1].get());
  ASSERT_NE(idExpr4, nullptr);
  EXPECT_EQ(idExpr4->getId(), "b");
  auto idExpr5 =
      llvm::dyn_cast<IdExprAST>(simpleStmtAssign4->getTargets()[2].get());
  ASSERT_NE(idExpr5, nullptr);
  EXPECT_EQ(idExpr5->getId(), "c");
  auto idExpr6 = llvm::dyn_cast<IdExprAST>(simpleStmtAssign4->getRhs());
  ASSERT_NE(idExpr6, nullptr);
  EXPECT_EQ(idExpr6->getId(), "d");

  auto simpleStmt5 = llvm::dyn_cast<SimpleStmtAST>(simpleStmts[4].get());
  ASSERT_NE(simpleStmt5, nullptr);
  auto simpleStmtAssign5 = llvm::dyn_cast<SimpleStmtAssignAST>(simpleStmt5);
  ASSERT_NE(simpleStmtAssign5, nullptr);
  EXPECT_EQ(simpleStmtAssign5->getTargets().size(), 1);
  auto targetExpr2 =
      llvm::dyn_cast<IdExprAST>(simpleStmtAssign5->getTargets()[0].get());
  ASSERT_NE(targetExpr2, nullptr);
  EXPECT_EQ(targetExpr2->getId(), "zz");
  EXPECT_EQ(simpleStmtAssign5->getRhs()->getKind(), ExprAST::Expr_ListLiteral);
  auto listLiteralExpr =
      llvm::dyn_cast<ListLiteralExprAST>(simpleStmtAssign5->getRhs());
  ASSERT_NE(listLiteralExpr, nullptr);
  const auto& listElements = listLiteralExpr->getElements();
  ASSERT_EQ(listElements.size(), 3);
  auto literalExpr3 = llvm::dyn_cast<LiteralExprAST>(listElements[0].get());
  ASSERT_NE(literalExpr3, nullptr);
  auto literalNumber3 =
      llvm::dyn_cast<LiteralNumberAST>(literalExpr3->getLiteral());
  ASSERT_NE(literalNumber3, nullptr);
  EXPECT_EQ(literalNumber3->getNumber(), 1);
  auto literalExpr4 = llvm::dyn_cast<LiteralExprAST>(listElements[1].get());
  ASSERT_NE(literalExpr4, nullptr);
  auto literalNumber4 =
      llvm::dyn_cast<LiteralNumberAST>(literalExpr4->getLiteral());
  ASSERT_NE(literalNumber4, nullptr);
  EXPECT_EQ(literalNumber4->getNumber(), 2);
  auto literalExpr5 = llvm::dyn_cast<LiteralExprAST>(listElements[2].get());
  ASSERT_NE(literalExpr5, nullptr);
  auto literalNumber5 =
      llvm::dyn_cast<LiteralNumberAST>(literalExpr5->getLiteral());
  ASSERT_NE(literalNumber5, nullptr);
  EXPECT_EQ(literalNumber5->getNumber(), 3);
}

TEST(ParserTest, TestSimpleBinaryExpressions) {
  std::string program = R"(
not a
-a
a + b
a - b
a * b
a // b
a % b
a == b
a != b
a < b
a > b
a <= b
a >= b
a and b
a or b
)";

  LexerBuffer lexer(program.c_str(), program.c_str() + program.size(),
                    "test.py");

  Parser parser(lexer);

  auto programAST = parser.parseProgram();
  ASSERT_NE(programAST, nullptr);
  const auto& simpleStmts = programAST->getStmts();
  ASSERT_EQ(simpleStmts.size(), 15);

  auto simpleStmt1 = llvm::dyn_cast<SimpleStmtAST>(simpleStmts[0].get());
  ASSERT_NE(simpleStmt1, nullptr);
  auto simpleStmt1Expr = llvm::dyn_cast<SimpleStmtExprAST>(simpleStmt1);
  ASSERT_NE(simpleStmt1Expr, nullptr);
  auto unaryExpr1 = llvm::dyn_cast<UnaryExprAST>(simpleStmt1Expr->getExpr());
  ASSERT_NE(unaryExpr1, nullptr);
  EXPECT_EQ(unaryExpr1->getOp(), TokenKind::k_not);

  auto simpleStmt2 = llvm::dyn_cast<SimpleStmtAST>(simpleStmts[1].get());
  ASSERT_NE(simpleStmt2, nullptr);
  auto simpleStmt2Expr = llvm::dyn_cast<SimpleStmtExprAST>(simpleStmt2);
  ASSERT_NE(simpleStmt2Expr, nullptr);
  auto unaryExpr2 = llvm::dyn_cast<UnaryExprAST>(simpleStmt2Expr->getExpr());
  ASSERT_NE(unaryExpr2, nullptr);
  EXPECT_EQ(unaryExpr2->getOp(), TokenKind::kMinus);

  EXPECT_EQ(llvm::dyn_cast<BinaryExprAST>(
                llvm::dyn_cast<SimpleStmtExprAST>(
                    llvm::dyn_cast<SimpleStmtAST>(simpleStmts[2].get()))
                    ->getExpr())
                ->getOp(),
            TokenKind::kPlus);
  EXPECT_EQ(llvm::dyn_cast<BinaryExprAST>(
                llvm::dyn_cast<SimpleStmtExprAST>(
                    llvm::dyn_cast<SimpleStmtAST>(simpleStmts[3].get()))
                    ->getExpr())
                ->getOp(),
            TokenKind::kMinus);
  EXPECT_EQ(llvm::dyn_cast<BinaryExprAST>(
                llvm::dyn_cast<SimpleStmtExprAST>(
                    llvm::dyn_cast<SimpleStmtAST>(simpleStmts[4].get()))
                    ->getExpr())
                ->getOp(),
            TokenKind::kMul);
  EXPECT_EQ(llvm::dyn_cast<BinaryExprAST>(
                llvm::dyn_cast<SimpleStmtExprAST>(
                    llvm::dyn_cast<SimpleStmtAST>(simpleStmts[5].get()))
                    ->getExpr())
                ->getOp(),
            TokenKind::kIntDiv);
  EXPECT_EQ(llvm::dyn_cast<BinaryExprAST>(
                llvm::dyn_cast<SimpleStmtExprAST>(
                    llvm::dyn_cast<SimpleStmtAST>(simpleStmts[6].get()))
                    ->getExpr())
                ->getOp(),
            TokenKind::kMod);
  EXPECT_EQ(llvm::dyn_cast<BinaryExprAST>(
                llvm::dyn_cast<SimpleStmtExprAST>(
                    llvm::dyn_cast<SimpleStmtAST>(simpleStmts[7].get()))
                    ->getExpr())
                ->getOp(),
            TokenKind::kEqual);
  EXPECT_EQ(llvm::dyn_cast<BinaryExprAST>(
                llvm::dyn_cast<SimpleStmtExprAST>(
                    llvm::dyn_cast<SimpleStmtAST>(simpleStmts[8].get()))
                    ->getExpr())
                ->getOp(),
            TokenKind::kInEqual);
  EXPECT_EQ(llvm::dyn_cast<BinaryExprAST>(
                llvm::dyn_cast<SimpleStmtExprAST>(
                    llvm::dyn_cast<SimpleStmtAST>(simpleStmts[9].get()))
                    ->getExpr())
                ->getOp(),
            TokenKind::kLessThan);
  EXPECT_EQ(llvm::dyn_cast<BinaryExprAST>(
                llvm::dyn_cast<SimpleStmtExprAST>(
                    llvm::dyn_cast<SimpleStmtAST>(simpleStmts[10].get()))
                    ->getExpr())
                ->getOp(),
            TokenKind::kGreaterThan);
  EXPECT_EQ(llvm::dyn_cast<BinaryExprAST>(
                llvm::dyn_cast<SimpleStmtExprAST>(
                    llvm::dyn_cast<SimpleStmtAST>(simpleStmts[11].get()))
                    ->getExpr())
                ->getOp(),
            TokenKind::kLessThanOrEqual);
  EXPECT_EQ(llvm::dyn_cast<BinaryExprAST>(
                llvm::dyn_cast<SimpleStmtExprAST>(
                    llvm::dyn_cast<SimpleStmtAST>(simpleStmts[12].get()))
                    ->getExpr())
                ->getOp(),
            TokenKind::kGreaterThanOrEqual);
  EXPECT_EQ(llvm::dyn_cast<BinaryExprAST>(
                llvm::dyn_cast<SimpleStmtExprAST>(
                    llvm::dyn_cast<SimpleStmtAST>(simpleStmts[13].get()))
                    ->getExpr())
                ->getOp(),
            TokenKind::k_and);
  EXPECT_EQ(llvm::dyn_cast<BinaryExprAST>(
                llvm::dyn_cast<SimpleStmtExprAST>(
                    llvm::dyn_cast<SimpleStmtAST>(simpleStmts[14].get()))
                    ->getExpr())
                ->getOp(),
            TokenKind::k_or);
}

// Test compound expressions
TEST(ParserTest, TestCompoundExpressions) {
  std::string program = R"(
a + b * 123
return a or b == 1
)";

  LexerBuffer lexer(program.c_str(), program.c_str() + program.size(),
                    "test.py");

  Parser parser(lexer);

  auto programAST = parser.parseProgram();
  ASSERT_NE(programAST, nullptr);
  const auto& simpleStmts = programAST->getStmts();
  ASSERT_EQ(simpleStmts.size(), 2);

  auto simpleStmt1 = llvm::dyn_cast<SimpleStmtAST>(simpleStmts[0].get());
  ASSERT_NE(simpleStmt1, nullptr);
  auto simpleStmt1Expr = llvm::dyn_cast<SimpleStmtExprAST>(simpleStmt1);
  ASSERT_NE(simpleStmt1Expr, nullptr);
  auto binaryExpr1 = llvm::dyn_cast<BinaryExprAST>(simpleStmt1Expr->getExpr());
  ASSERT_NE(binaryExpr1, nullptr);
  EXPECT_EQ(binaryExpr1->getOp(), TokenKind::kPlus);
  auto rhsExpr = llvm::dyn_cast<BinaryExprAST>(binaryExpr1->getRhs());
  ASSERT_NE(rhsExpr, nullptr);
  EXPECT_EQ(rhsExpr->getOp(), TokenKind::kMul);

  auto simpleStmt2 = llvm::dyn_cast<SimpleStmtAST>(simpleStmts[1].get());
  ASSERT_NE(simpleStmt2, nullptr);
  auto simpleStmt2Expr = llvm::dyn_cast<SimpleStmtReturnAST>(simpleStmt2);
  ASSERT_NE(simpleStmt2Expr, nullptr);
  auto binaryExpr2 = llvm::dyn_cast<BinaryExprAST>(simpleStmt2Expr->getExpr());
  ASSERT_NE(binaryExpr2, nullptr);
  EXPECT_EQ(binaryExpr2->getOp(), TokenKind::k_or);
  auto binaryExpr3 = llvm::dyn_cast<BinaryExprAST>(binaryExpr2->getRhs());
  ASSERT_NE(binaryExpr3, nullptr);
  EXPECT_EQ(binaryExpr3->getOp(), TokenKind::kEqual);
}

// Associativity test (((a == b) and c) or d)
//             or
//            /  \
//           /    \
//          /      \
//         /        \
//        and        \
//       /  \         \
//      ==   \         \
//     /  \   \         \
//    /    \   \         \
//   a      b   c         d
TEST(ParserTest, TestAssociativity) {
  std::string program = R"(
a == b and c or d
)";

  LexerBuffer lexer(program.c_str(), program.c_str() + program.size(),
                    "test.py");

  Parser parser(lexer);

  auto programAST = parser.parseProgram();
  ASSERT_NE(programAST, nullptr);
  const auto& simpleStmts = programAST->getStmts();
  ASSERT_EQ(simpleStmts.size(), 1);

  auto simpleStmt1 = llvm::dyn_cast<SimpleStmtAST>(simpleStmts[0].get());
  ASSERT_NE(simpleStmt1, nullptr);
  auto simpleStmt1Expr = llvm::dyn_cast<SimpleStmtExprAST>(simpleStmt1);
  ASSERT_NE(simpleStmt1Expr, nullptr);
  auto binaryExpr1 = llvm::dyn_cast<BinaryExprAST>(simpleStmt1Expr->getExpr());
  ASSERT_NE(binaryExpr1, nullptr);
  EXPECT_EQ(binaryExpr1->getOp(), TokenKind::k_or);
  auto binaryExpr2 = llvm::dyn_cast<BinaryExprAST>(binaryExpr1->getLhs());
  ASSERT_NE(binaryExpr2, nullptr);
  EXPECT_EQ(binaryExpr2->getOp(), TokenKind::k_and);
  auto binaryExpr3 = llvm::dyn_cast<BinaryExprAST>(binaryExpr2->getLhs());
  ASSERT_NE(binaryExpr3, nullptr);
  EXPECT_EQ(binaryExpr3->getOp(), TokenKind::kEqual);
  auto lhsExpr = llvm::dyn_cast<IdExprAST>(binaryExpr3->getLhs());
  ASSERT_NE(lhsExpr, nullptr);
}

// Test call expression (function call or constructor call)
TEST(ParserTest, TestCallExpression) {
  std::string program = R"(
foo(bar, True, [1,1])
a.c.create(12, True)
)";

  LexerBuffer lexer(program.c_str(), program.c_str() + program.size(),
                    "test.py");

  Parser parser(lexer);

  auto programAST = parser.parseProgram();
  ASSERT_NE(programAST, nullptr);
  const auto& simpleStmts = programAST->getStmts();
  ASSERT_EQ(simpleStmts.size(), 2);

  auto simpleStmt1 = llvm::dyn_cast<SimpleStmtAST>(simpleStmts[0].get());
  ASSERT_NE(simpleStmt1, nullptr);
  auto simpleStmt1Expr = llvm::dyn_cast<SimpleStmtExprAST>(simpleStmt1);
  ASSERT_NE(simpleStmt1Expr, nullptr);
  auto callExpr = llvm::dyn_cast<CallExprAST>(simpleStmt1Expr->getExpr());
  ASSERT_NE(callExpr, nullptr);
  EXPECT_EQ(callExpr->getArgs().size(), 3);
  auto idExpr = llvm::dyn_cast<IdExprAST>(callExpr->getCallee());
  ASSERT_NE(idExpr, nullptr);
  EXPECT_EQ(idExpr->getId(), "foo");

  auto simpleStmt2 = llvm::dyn_cast<SimpleStmtAST>(simpleStmts[1].get());
  ASSERT_NE(simpleStmt2, nullptr);
  auto simpleStmt2Expr = llvm::dyn_cast<SimpleStmtExprAST>(simpleStmt2);
  ASSERT_NE(simpleStmt2Expr, nullptr);
  auto binaryExpr = llvm::dyn_cast<BinaryExprAST>(simpleStmt2Expr->getExpr());
  ASSERT_NE(binaryExpr, nullptr);
  EXPECT_EQ(binaryExpr->getOp(), TokenKind::kAttrAccessOp);
  auto callExpr2 = llvm::dyn_cast<CallExprAST>(binaryExpr->getRhs());
  ASSERT_NE(callExpr2, nullptr);

  auto binaryExpr2 = llvm::dyn_cast<BinaryExprAST>(binaryExpr->getLhs());
  ASSERT_NE(binaryExpr2, nullptr);
  EXPECT_EQ(binaryExpr2->getOp(), TokenKind::kAttrAccessOp);
}

// Test call expression inside a composed expression
TEST(ParserTest, TestCallExpressionInComposedExpr) {
  std::string program = R"(
a + a[1].bar(x)
)";

  LexerBuffer lexer(program.c_str(), program.c_str() + program.size(),
                    "test.py");

  Parser parser(lexer);

  auto programAST = parser.parseProgram();
  ASSERT_NE(programAST, nullptr);
  const auto& simpleStmts = programAST->getStmts();
  ASSERT_EQ(simpleStmts.size(), 1);

  auto simpleStmt1 = llvm::dyn_cast<SimpleStmtAST>(simpleStmts[0].get());
  ASSERT_NE(simpleStmt1, nullptr);
  auto simpleStmt1Expr = llvm::dyn_cast<SimpleStmtExprAST>(simpleStmt1);
  ASSERT_NE(simpleStmt1Expr, nullptr);
  auto binaryExpr1 = llvm::dyn_cast<BinaryExprAST>(simpleStmt1Expr->getExpr());
  ASSERT_NE(binaryExpr1, nullptr);
  EXPECT_EQ(binaryExpr1->getOp(), TokenKind::kPlus);
  auto lhsExpr = llvm::dyn_cast<IdExprAST>(binaryExpr1->getLhs());
  ASSERT_NE(lhsExpr, nullptr);
  EXPECT_EQ(lhsExpr->getId(), "a");
  auto rhsExpr = llvm::dyn_cast<BinaryExprAST>(binaryExpr1->getRhs());
  ASSERT_NE(rhsExpr, nullptr);
  EXPECT_EQ(rhsExpr->getOp(), TokenKind::kAttrAccessOp);
  auto rhsLhsExpr = llvm::dyn_cast<BinaryExprAST>(rhsExpr->getLhs());
  ASSERT_NE(rhsLhsExpr, nullptr);
  EXPECT_EQ(rhsLhsExpr->getOp(), TokenKind::kIndexAccessOp);

  auto rhsRhsExpr = llvm::dyn_cast<CallExprAST>(rhsExpr->getRhs());
  ASSERT_NE(rhsRhsExpr, nullptr);
  auto rhsRhsCallee = llvm::dyn_cast<IdExprAST>(rhsRhsExpr->getCallee());
  ASSERT_NE(rhsRhsCallee, nullptr);
  EXPECT_EQ(rhsRhsCallee->getId(), "bar");
  EXPECT_EQ(rhsRhsExpr->getArgs().size(), 1);
}

TEST(ParserTest, TestIfStatement) {
  std::string program = R"(
if a + b == 0:
    print("a")
elif a + b == 1:
    print("b")
    return x
elif a + b == 2:
    print("c")
    v = u  + a * t
else:
    print("d")
    return y
)";

  LexerBuffer lexer(program.c_str(), program.c_str() + program.size(),
                    "test.py");
  Parser parser(lexer);

  auto programAST = parser.parseProgram();
  ASSERT_NE(programAST, nullptr);
  const auto& simpleStmts = programAST->getStmts();
  ASSERT_EQ(simpleStmts.size(), 1);
  auto simpleStmt1 = llvm::dyn_cast<StmtIfAST>(simpleStmts[0].get());
  ASSERT_NE(simpleStmt1, nullptr);
  EXPECT_EQ(simpleStmt1->getElifs().size(), 2);
  EXPECT_EQ(simpleStmt1->getElseBody().size(), 2);
  EXPECT_EQ(simpleStmt1->getBody().size(), 1);
  EXPECT_EQ(simpleStmt1->getElifs()[0]->getBody().size(), 2);
  EXPECT_EQ(simpleStmt1->getElifs()[1]->getBody().size(), 2);
}

TEST(ParserTest, TestWhileStatement) {
  std::string program = R"(
while v == 0:
    print(v)
    v = u - a * t
)";

  LexerBuffer lexer(program.c_str(), program.c_str() + program.size(),
                    "test.py");
  Parser parser(lexer);

  auto programAST = parser.parseProgram();
  ASSERT_NE(programAST, nullptr);
  const auto& simpleStmts = programAST->getStmts();
  ASSERT_EQ(simpleStmts.size(), 1);
  auto simpleStmt1 = llvm::dyn_cast<StmtWhileAST>(simpleStmts[0].get());
  ASSERT_NE(simpleStmt1, nullptr);
  EXPECT_EQ(simpleStmt1->getBody().size(), 2);
}

TEST(ParserTest, TestForStatement) {
  std::string program = R"(
for ball in [1, 3, 5]:
    print(ball)
for cStr in "string literal":
    print(cStr + "!\n")
)";

  LexerBuffer lexer(program.c_str(), program.c_str() + program.size(),
                    "test.py");
  Parser parser(lexer);

  auto programAST = parser.parseProgram();
  ASSERT_NE(programAST, nullptr);
  const auto& simpleStmts = programAST->getStmts();
  ASSERT_EQ(simpleStmts.size(), 2);
  auto simpleStmt1 = llvm::dyn_cast<StmtForAST>(simpleStmts[0].get());
  ASSERT_NE(simpleStmt1, nullptr);
  EXPECT_EQ(simpleStmt1->getBody().size(), 1);
  auto simpleStmt2 = llvm::dyn_cast<StmtForAST>(simpleStmts[1].get());
  ASSERT_NE(simpleStmt2, nullptr);
  EXPECT_EQ(simpleStmt2->getBody().size(), 1);
}

TEST(ParserTest, TestNestedControlFlows) {
  std::string program = R"(
while x > 0:
    x = x -1
    print(x)
    while x > 3:
        if list[x] % 2 == 0:
            print("hooray")
        else:
            print("missed")
    for c in "chocopy":
        print(c)
)";

  LexerBuffer lexer(program.c_str(), program.c_str() + program.size(),
                    "test.py");
  Parser parser(lexer);

  auto programAST = parser.parseProgram();
  ASSERT_NE(programAST, nullptr);
  const auto& simpleStmts = programAST->getStmts();
  ASSERT_EQ(simpleStmts.size(), 1);
  auto simpleStmt1 = llvm::dyn_cast<StmtWhileAST>(simpleStmts[0].get());
  ASSERT_NE(simpleStmt1, nullptr);
  EXPECT_EQ(simpleStmt1->getBody().size(), 4);

  auto simpleStmt2 =
      llvm::dyn_cast<StmtWhileAST>(simpleStmt1->getBody()[2].get());
  ASSERT_NE(simpleStmt2, nullptr);
  EXPECT_EQ(simpleStmt2->getBody().size(), 1);
  auto innerIfStmt = llvm::dyn_cast<StmtIfAST>(simpleStmt2->getBody()[0].get());
  ASSERT_NE(innerIfStmt, nullptr);
  EXPECT_EQ(innerIfStmt->getBody().size(), 1);
  EXPECT_EQ(innerIfStmt->getElifs().size(), 0);
  EXPECT_EQ(innerIfStmt->getElseBody().size(), 1);

  auto simpleStmt3 =
      llvm::dyn_cast<StmtForAST>(simpleStmt1->getBody()[3].get());
  ASSERT_NE(simpleStmt3, nullptr);
  EXPECT_EQ(simpleStmt3->getBody().size(), 1);
}

TEST(ParserTest, TestFunctionDefinition) {
  std::string program = R"(
def foo(x: int, y: int) -> int:
    global a
    nonlocal b
    
    j: int = 2
    k: int = 3
    
    def bar() -> int:
        return a + a * 2
         
    if k == j + i:
        return x + y
    return x - y

def baz():
    pass
)";
  LexerBuffer lexer(program.c_str(), program.c_str() + program.size(),
                    "test.py");
  Parser parser(lexer);

  auto programAST = parser.parseProgram();
  ASSERT_NE(programAST, nullptr);
  const auto& funcDefs = programAST->getFuncDefs();
  ASSERT_EQ(funcDefs.size(), 2);
  auto funcDef = llvm::dyn_cast<FunctionAST>(funcDefs[0].get());
  ASSERT_NE(funcDef, nullptr);
  EXPECT_EQ(funcDef->getId(), "foo");
  EXPECT_EQ(funcDef->getGlobalDecls().size(), 1);
  EXPECT_EQ(funcDef->getNonlocalDecls().size(), 1);
  EXPECT_EQ(funcDef->getBody().size(), 2);
  EXPECT_EQ(funcDef->getArgs().size(), 2);
  EXPECT_EQ(funcDef->getFuncDefs().size(), 1);

  auto funcDef2 = llvm::dyn_cast<FunctionAST>(funcDefs[1].get());
  ASSERT_NE(funcDef2, nullptr);
  EXPECT_EQ(funcDef2->getId(), "baz");
  EXPECT_EQ(funcDef2->getGlobalDecls().size(), 0);
  EXPECT_EQ(funcDef2->getNonlocalDecls().size(), 0);
  EXPECT_EQ(funcDef2->getBody().size(), 1);
  EXPECT_EQ(funcDef2->getArgs().size(), 0);
  EXPECT_EQ(funcDef2->getFuncDefs().size(), 0);
  EXPECT_EQ(funcDef2->getReturnType(), nullptr);
}

TEST(ParserTest, TestClassWithPassBody) {
  std::string program = R"(
class foo(object):
    pass
)";
  LexerBuffer lexer(program.c_str(), program.c_str() + program.size(),
                    "test.py");
  Parser parser(lexer);

  auto programAST = parser.parseProgram();
  ASSERT_NE(programAST, nullptr);
  const auto& classDefs = programAST->getClassDefs();
  ASSERT_EQ(classDefs.size(), 1);
  auto classDef = classDefs[0].get();
  ASSERT_NE(classDef, nullptr);
  EXPECT_EQ(classDef->getId(), "foo");
  EXPECT_EQ(classDef->getSuperClassId(), "object");
  EXPECT_EQ(classDef->getMethodDefs().size(), 0);
  EXPECT_EQ(classDef->getVarDefs().size(), 0);
}

TEST(ParserTest, TestClassDefinition) {
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

c:animal = None
c = cow()
c.make_noise() 
)";

  LexerBuffer lexer(program.c_str(), program.c_str() + program.size(),
                    "test.py");
  Parser parser(lexer);

  auto programAST = parser.parseProgram();
  ASSERT_NE(programAST, nullptr);
  const auto& classDefs = programAST->getClassDefs();
  ASSERT_EQ(classDefs.size(), 2);
  auto classDef = classDefs[0].get();
  ASSERT_NE(classDef, nullptr);
  EXPECT_EQ(classDef->getId(), "animal");
  EXPECT_EQ(classDef->getSuperClassId(), "object");
  EXPECT_EQ(classDef->getMethodDefs().size(), 2);
  EXPECT_EQ(classDef->getVarDefs().size(), 1);

  auto classDef2 = classDefs[1].get();
  ASSERT_NE(classDef2, nullptr);
  EXPECT_EQ(classDef2->getId(), "cow");
  EXPECT_EQ(classDef2->getSuperClassId(), "animal");
  EXPECT_EQ(classDef2->getMethodDefs().size(), 2);
  EXPECT_EQ(classDef2->getVarDefs().size(), 0);
}

} // namespace chocopy
