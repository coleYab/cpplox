#include "token.h"
#include <gtest/gtest.h>

namespace cpplox {

TEST(TokenTest, ConstructorAndGetters) {
    Token t(TokenType::NUMBER, "42", 42.0, 1);
    EXPECT_EQ(t.getType(), TokenType::NUMBER);
    EXPECT_EQ(t.getLexeme(), "42");
    EXPECT_EQ(t.getLine(), 1);
    EXPECT_TRUE(t.getLiteral().has_value());
    EXPECT_DOUBLE_EQ(std::any_cast<double>(t.getLiteral()), 42.0);
}

TEST(TokenTest, ConstructorWithNoLiteral) {
    Token t(TokenType::PLUS, "+", nullptr, 3);
    EXPECT_EQ(t.getType(), TokenType::PLUS);
    EXPECT_EQ(t.getLexeme(), "+");
    EXPECT_EQ(t.getLine(), 3);
}

TEST(TokenTest, ConstructorWithStringLiteral) {
    Token t(TokenType::STRING, "\"hello\"", std::string("hello"), 5);
    EXPECT_EQ(t.getType(), TokenType::STRING);
    EXPECT_EQ(std::any_cast<std::string>(t.getLiteral()), "hello");
}

TEST(TokenTest, ToString) {
    Token t(TokenType::NUMBER, "42", 42.0, 1);
    std::string s = t.toString();
    EXPECT_EQ(s, "21 42 1");
}

} // namespace cpplox
