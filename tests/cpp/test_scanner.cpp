#include "scanner.h"
#include <gtest/gtest.h>

namespace cpplox {

TEST(ScannerTest, SingleCharacterTokens) {
    Scanner scanner("(){},.-+;*");
    Tokens tokens = scanner.scan();
    EXPECT_EQ(tokens.size(), 11);
    EXPECT_EQ(tokens[0].getType(), TokenType::LEFT_PAREN);
    EXPECT_EQ(tokens[1].getType(), TokenType::RIGHT_PAREN);
    EXPECT_EQ(tokens[2].getType(), TokenType::LEFT_BRACE);
    EXPECT_EQ(tokens[3].getType(), TokenType::RIGHT_BRACE);
    EXPECT_EQ(tokens[4].getType(), TokenType::COMMA);
    EXPECT_EQ(tokens[5].getType(), TokenType::DOT);
    EXPECT_EQ(tokens[6].getType(), TokenType::MINUS);
    EXPECT_EQ(tokens[7].getType(), TokenType::PLUS);
    EXPECT_EQ(tokens[8].getType(), TokenType::SEMICOLON);
    EXPECT_EQ(tokens[9].getType(), TokenType::STAR);
    EXPECT_EQ(tokens[10].getType(), TokenType::EOFF);
}

TEST(ScannerTest, TwoCharacterTokens) {
    Scanner scanner("!= == <= >=");
    Tokens tokens = scanner.scan();
    ASSERT_EQ(tokens.size(), 5);
    EXPECT_EQ(tokens[0].getType(), TokenType::BANG_EQUAL);
    EXPECT_EQ(tokens[1].getType(), TokenType::EQUAL_EQUAL);
    EXPECT_EQ(tokens[2].getType(), TokenType::LESS_EQUAL);
    EXPECT_EQ(tokens[3].getType(), TokenType::GREATER_EQUAL);
    EXPECT_EQ(tokens[4].getType(), TokenType::EOFF);
}

TEST(ScannerTest, SingleAndTwoCharacterTokens) {
    Scanner scanner("! = < >");
    Tokens tokens = scanner.scan();
    ASSERT_EQ(tokens.size(), 5);
    EXPECT_EQ(tokens[0].getType(), TokenType::BANG);
    EXPECT_EQ(tokens[1].getType(), TokenType::EQUAL);
    EXPECT_EQ(tokens[2].getType(), TokenType::LESS);
    EXPECT_EQ(tokens[3].getType(), TokenType::GREATER);
    EXPECT_EQ(tokens[4].getType(), TokenType::EOFF);
}

TEST(ScannerTest, StringLiteral) {
    Scanner scanner("\"hello\"");
    Tokens tokens = scanner.scan();
    ASSERT_EQ(tokens.size(), 2);
    EXPECT_EQ(tokens[0].getType(), TokenType::STRING);
    EXPECT_EQ(std::any_cast<std::string>(tokens[0].getLiteral()), "hello");
}

TEST(ScannerTest, StringLiteralEmpty) {
    Scanner scanner("\"\"");
    Tokens tokens = scanner.scan();
    ASSERT_EQ(tokens.size(), 2);
    EXPECT_EQ(tokens[0].getType(), TokenType::STRING);
    EXPECT_EQ(std::any_cast<std::string>(tokens[0].getLiteral()), "");
}

TEST(ScannerTest, StringLiteralUnterminated) {
    hasError_ = false;
    Scanner scanner("\"hello");
    scanner.scan();
    EXPECT_TRUE(hasError_);
    hasError_ = false;
}

TEST(ScannerTest, NumberLiteral) {
    Scanner scanner("42");
    Tokens tokens = scanner.scan();
    ASSERT_EQ(tokens.size(), 2);
    EXPECT_EQ(tokens[0].getType(), TokenType::NUMBER);
    EXPECT_DOUBLE_EQ(std::any_cast<double>(tokens[0].getLiteral()), 42.0);
}

TEST(ScannerTest, NumberLiteralDecimal) {
    Scanner scanner("3.14");
    Tokens tokens = scanner.scan();
    ASSERT_EQ(tokens.size(), 2);
    EXPECT_EQ(tokens[0].getType(), TokenType::NUMBER);
    EXPECT_DOUBLE_EQ(std::any_cast<double>(tokens[0].getLiteral()), 3.14);
}

TEST(ScannerTest, Identifier) {
    Scanner scanner("foo");
    Tokens tokens = scanner.scan();
    ASSERT_EQ(tokens.size(), 2);
    EXPECT_EQ(tokens[0].getType(), TokenType::IDENTIFIER);
    EXPECT_EQ(tokens[0].getLexeme(), "foo");
}

TEST(ScannerTest, Keywords) {
    Scanner scanner("and class else false fun for if nil or print return super this true var while");
    Tokens tokens = scanner.scan();
    ASSERT_EQ(tokens.size(), 17);
    EXPECT_EQ(tokens[0].getType(), TokenType::AND);
    EXPECT_EQ(tokens[1].getType(), TokenType::CLASS);
    EXPECT_EQ(tokens[2].getType(), TokenType::ELSE);
    EXPECT_EQ(tokens[3].getType(), TokenType::FALSE);
    EXPECT_EQ(tokens[4].getType(), TokenType::FUN);
    EXPECT_EQ(tokens[5].getType(), TokenType::FOR);
    EXPECT_EQ(tokens[6].getType(), TokenType::IF);
    EXPECT_EQ(tokens[7].getType(), TokenType::NIL);
    EXPECT_EQ(tokens[8].getType(), TokenType::OR);
    EXPECT_EQ(tokens[9].getType(), TokenType::PRINT);
    EXPECT_EQ(tokens[10].getType(), TokenType::RETURN);
    EXPECT_EQ(tokens[11].getType(), TokenType::SUPER);
    EXPECT_EQ(tokens[12].getType(), TokenType::THIS);
    EXPECT_EQ(tokens[13].getType(), TokenType::TRUE);
    EXPECT_EQ(tokens[14].getType(), TokenType::VAR);
    EXPECT_EQ(tokens[15].getType(), TokenType::WHILE);
}

TEST(ScannerTest, LineComment) {
    hasError_ = false;
    Scanner scanner("// this is a comment\n42");
    Tokens tokens = scanner.scan();
    ASSERT_EQ(tokens.size(), 2);
    EXPECT_EQ(tokens[0].getType(), TokenType::NUMBER);
    EXPECT_EQ(tokens[0].getLine(), 2);
    EXPECT_EQ(tokens[1].getType(), TokenType::EOFF);
}

TEST(ScannerTest, BlockComment) {
    hasError_ = false;
    Scanner scanner("/* comment */42");
    Tokens tokens = scanner.scan();
    ASSERT_EQ(tokens.size(), 2);
    EXPECT_EQ(tokens[0].getType(), TokenType::NUMBER);
}

TEST(ScannerTest, BlockCommentUnclosed) {
    hasError_ = false;
    Scanner scanner("/* comment");
    scanner.scan();
    EXPECT_TRUE(hasError_);
    hasError_ = false;
}

TEST(ScannerTest, Whitespace) {
    Scanner scanner("  \t\r\n42");
    Tokens tokens = scanner.scan();
    ASSERT_EQ(tokens.size(), 2);
    EXPECT_EQ(tokens[0].getType(), TokenType::NUMBER);
    EXPECT_EQ(tokens[0].getLine(), 2);
}

TEST(ScannerTest, UnexpectedChar) {
    hasError_ = false;
    Scanner scanner("@");
    scanner.scan();
    EXPECT_TRUE(hasError_);
    hasError_ = false;
}

TEST(ScannerTest, EmptyInput) {
    hasError_ = false;
    Scanner scanner("");
    Tokens tokens = scanner.scan();
    ASSERT_EQ(tokens.size(), 1);
    EXPECT_EQ(tokens[0].getType(), TokenType::EOFF);
}

TEST(ScannerTest, Slash) {
    Scanner scanner("/");
    Tokens tokens = scanner.scan();
    ASSERT_EQ(tokens.size(), 2);
    EXPECT_EQ(tokens[0].getType(), TokenType::SLASH);
}

TEST(ScannerTest, NewlinesIncrementLine) {
    Scanner scanner("\n\n42");
    Tokens tokens = scanner.scan();
    ASSERT_EQ(tokens.size(), 2);
    EXPECT_EQ(tokens[0].getLine(), 3);
}

} // namespace cpplox
