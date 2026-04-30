#include "ast_printer.h"
#include <gtest/gtest.h>

namespace cpplox {

TEST(AstPrinterTest, LiteralNumber) {
    auto expr = std::make_shared<Literal>(42.0);
    AstPrinter printer;
    EXPECT_EQ(printer.print(expr), "42.000000");
}

TEST(AstPrinterTest, LiteralString) {
    auto expr = std::make_shared<Literal>(std::string("hello"));
    AstPrinter printer;
    EXPECT_EQ(printer.print(expr), "hello");
}

TEST(AstPrinterTest, LiteralTrue) {
    auto expr = std::make_shared<Literal>(true);
    AstPrinter printer;
    EXPECT_EQ(printer.print(expr), "true");
}

TEST(AstPrinterTest, LiteralFalse) {
    auto expr = std::make_shared<Literal>(false);
    AstPrinter printer;
    EXPECT_EQ(printer.print(expr), "false");
}

TEST(AstPrinterTest, LiteralNil) {
    auto expr = std::make_shared<Literal>(std::any{});
    AstPrinter printer;
    EXPECT_EQ(printer.print(expr), "nil");
}

TEST(AstPrinterTest, UnaryMinus) {
    Token minus(TokenType::MINUS, "-", nullptr, 1);
    auto right = std::make_shared<Literal>(42.0);
    auto expr = std::make_shared<Unary>(minus, right);
    AstPrinter printer;
    EXPECT_EQ(printer.print(expr), "(- 42.000000)");
}

TEST(AstPrinterTest, UnaryBang) {
    Token bang(TokenType::BANG, "!", nullptr, 1);
    auto right = std::make_shared<Literal>(true);
    auto expr = std::make_shared<Unary>(bang, right);
    AstPrinter printer;
    EXPECT_EQ(printer.print(expr), "(! true)");
}

TEST(AstPrinterTest, Binary) {
    Token plus(TokenType::PLUS, "+", nullptr, 1);
    auto left = std::make_shared<Literal>(1.0);
    auto right = std::make_shared<Literal>(2.0);
    auto expr = std::make_shared<Binary>(left, plus, right);
    AstPrinter printer;
    EXPECT_EQ(printer.print(expr), "(+ 1.000000 2.000000)");
}

TEST(AstPrinterTest, Grouping) {
    auto inner = std::make_shared<Literal>(42.0);
    auto expr = std::make_shared<Grouping>(inner);
    AstPrinter printer;
    EXPECT_EQ(printer.print(expr), "(group 42.000000)");
}

TEST(AstPrinterTest, ComplexExpression) {
    Token star(TokenType::STAR, "*", nullptr, 1);
    Token plus(TokenType::PLUS, "+", nullptr, 1);
    Token minus(TokenType::MINUS, "-", nullptr, 1);

    auto a = std::make_shared<Literal>(1.0);
    auto b = std::make_shared<Literal>(2.0);
    auto c = std::make_shared<Literal>(3.0);
    auto d = std::make_shared<Literal>(4.0);

    auto left = std::make_shared<Binary>(a, star, b);
    auto right = std::make_shared<Binary>(c, minus, d);
    auto expr = std::make_shared<Binary>(left, plus, right);

    AstPrinter printer;
    EXPECT_EQ(printer.print(expr), "(+ (* 1.000000 2.000000) (- 3.000000 4.000000))");
}

} // namespace cpplox
