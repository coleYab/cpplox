#include "interpreter.h"
#include "scanner.h"
#include "parser.h"
#include <gtest/gtest.h>
#include <sstream>

namespace cpplox {

class InterpreterTest : public ::testing::Test {
protected:
    void SetUp() override {
        hasError_ = false;
    }
};

TEST_F(InterpreterTest, InterpretLiteral) {
    auto interpreter = std::make_shared<Interpreter>();

    auto expr = std::make_shared<Literal>(42.0);
    auto val = interpreter->visitLiteralExpr(*expr);
    EXPECT_DOUBLE_EQ(std::any_cast<double>(val), 42.0);
}

TEST_F(InterpreterTest, InterpretGrouping) {
    auto interpreter = std::make_shared<Interpreter>();

    auto inner = std::make_shared<Literal>(42.0);
    auto expr = std::make_shared<Grouping>(inner);
    auto val = interpreter->visitGroupingExpr(*expr);
    EXPECT_DOUBLE_EQ(std::any_cast<double>(val), 42.0);
}

TEST_F(InterpreterTest, InterpretUnaryMinus) {
    auto interpreter = std::make_shared<Interpreter>();

    Token minus(TokenType::MINUS, "-", nullptr, 1);
    auto right = std::make_shared<Literal>(42.0);
    auto expr = std::make_shared<Unary>(minus, right);
    auto val = interpreter->visitUnaryExpr(*expr);
    EXPECT_DOUBLE_EQ(std::any_cast<double>(val), -42.0);
}

TEST_F(InterpreterTest, InterpretUnaryBang) {
    auto interpreter = std::make_shared<Interpreter>();

    Token bang(TokenType::BANG, "!", nullptr, 1);
    auto right = std::make_shared<Literal>(false);
    auto expr = std::make_shared<Unary>(bang, right);
    auto val = interpreter->visitUnaryExpr(*expr);
    EXPECT_TRUE(std::any_cast<bool>(val));
}

TEST_F(InterpreterTest, InterpretBinaryPlus) {
    auto interpreter = std::make_shared<Interpreter>();

    Token plus(TokenType::PLUS, "+", nullptr, 1);
    auto left = std::make_shared<Literal>(1.0);
    auto right = std::make_shared<Literal>(2.0);
    auto expr = std::make_shared<Binary>(left, plus, right);
    auto val = interpreter->visitBinaryExpr(*expr);
    EXPECT_DOUBLE_EQ(std::any_cast<double>(val), 3.0);
}

TEST_F(InterpreterTest, InterpretBinaryStringConcat) {
    auto interpreter = std::make_shared<Interpreter>();

    Token plus(TokenType::PLUS, "+", nullptr, 1);
    auto left = std::make_shared<Literal>(std::string("hello "));
    auto right = std::make_shared<Literal>(std::string("world"));
    auto expr = std::make_shared<Binary>(left, plus, right);
    auto val = interpreter->visitBinaryExpr(*expr);
    EXPECT_EQ(std::any_cast<std::string>(val), "hello world");
}

TEST_F(InterpreterTest, InterpretBinaryMinus) {
    auto interpreter = std::make_shared<Interpreter>();

    Token minus(TokenType::MINUS, "-", nullptr, 1);
    auto left = std::make_shared<Literal>(5.0);
    auto right = std::make_shared<Literal>(3.0);
    auto expr = std::make_shared<Binary>(left, minus, right);
    auto val = interpreter->visitBinaryExpr(*expr);
    EXPECT_DOUBLE_EQ(std::any_cast<double>(val), 2.0);
}

TEST_F(InterpreterTest, InterpretBinaryStar) {
    auto interpreter = std::make_shared<Interpreter>();

    Token star(TokenType::STAR, "*", nullptr, 1);
    auto left = std::make_shared<Literal>(3.0);
    auto right = std::make_shared<Literal>(4.0);
    auto expr = std::make_shared<Binary>(left, star, right);
    auto val = interpreter->visitBinaryExpr(*expr);
    EXPECT_DOUBLE_EQ(std::any_cast<double>(val), 12.0);
}

TEST_F(InterpreterTest, InterpretBinarySlash) {
    auto interpreter = std::make_shared<Interpreter>();

    Token slash(TokenType::SLASH, "/", nullptr, 1);
    auto left = std::make_shared<Literal>(10.0);
    auto right = std::make_shared<Literal>(2.0);
    auto expr = std::make_shared<Binary>(left, slash, right);
    auto val = interpreter->visitBinaryExpr(*expr);
    EXPECT_DOUBLE_EQ(std::any_cast<double>(val), 5.0);
}

TEST_F(InterpreterTest, InterpretBinaryEqualEqual) {
    auto interpreter = std::make_shared<Interpreter>();

    Token eq(TokenType::EQUAL_EQUAL, "==", nullptr, 1);
    auto left = std::make_shared<Literal>(1.0);
    auto right = std::make_shared<Literal>(1.0);
    auto expr = std::make_shared<Binary>(left, eq, right);
    auto val = interpreter->visitBinaryExpr(*expr);
    EXPECT_TRUE(std::any_cast<bool>(val));
}

TEST_F(InterpreterTest, InterpretBinaryBangEqual) {
    auto interpreter = std::make_shared<Interpreter>();

    Token neq(TokenType::BANG_EQUAL, "!=", nullptr, 1);
    auto left = std::make_shared<Literal>(1.0);
    auto right = std::make_shared<Literal>(2.0);
    auto expr = std::make_shared<Binary>(left, neq, right);
    auto val = interpreter->visitBinaryExpr(*expr);
    EXPECT_TRUE(std::any_cast<bool>(val));
}

TEST_F(InterpreterTest, InterpretLogicalAnd) {
    auto interpreter = std::make_shared<Interpreter>();

    Token op(TokenType::AND, "and", nullptr, 1);
    auto left = std::make_shared<Literal>(true);
    auto right = std::make_shared<Literal>(false);
    auto expr = std::make_shared<Logical>(left, op, right);
    auto val = interpreter->visitLogicalExpr(*expr);
    EXPECT_FALSE(std::any_cast<bool>(val));
}

TEST_F(InterpreterTest, InterpretLogicalOr) {
    auto interpreter = std::make_shared<Interpreter>();

    Token op(TokenType::OR, "or", nullptr, 1);
    auto left = std::make_shared<Literal>(false);
    auto right = std::make_shared<Literal>(true);
    auto expr = std::make_shared<Logical>(left, op, right);
    auto val = interpreter->visitLogicalExpr(*expr);
    EXPECT_TRUE(std::any_cast<bool>(val));
}

TEST_F(InterpreterTest, InterpretVarAndExpressionStmt) {
    Scanner scanner("var x = 42; print x;");
    Tokens tokens = scanner.scan();
    Parser parser(tokens);
    Stmts stmts = parser.parse();

    auto interpreter = std::make_shared<Interpreter>();
    EXPECT_NO_THROW(interpreter->interpret(stmts));
}

TEST_F(InterpreterTest, InterpretIfStmt) {
    Scanner scanner("if (true) print 1; else print 2;");
    Tokens tokens = scanner.scan();
    Parser parser(tokens);
    Stmts stmts = parser.parse();

    auto interpreter = std::make_shared<Interpreter>();
    EXPECT_NO_THROW(interpreter->interpret(stmts));
}

TEST_F(InterpreterTest, InterpretWhileStmt) {
    Scanner scanner("var x = 0; while (x < 3) { print x; x = x + 1; }");
    Tokens tokens = scanner.scan();
    Parser parser(tokens);
    Stmts stmts = parser.parse();

    auto interpreter = std::make_shared<Interpreter>();
    EXPECT_NO_THROW(interpreter->interpret(stmts));
}

TEST_F(InterpreterTest, InterpretFunction) {
    Scanner scanner("fun f() { print 42; } f();");
    Tokens tokens = scanner.scan();
    Parser parser(tokens);
    Stmts stmts = parser.parse();

    auto interpreter = std::make_shared<Interpreter>();
    EXPECT_NO_THROW(interpreter->interpret(stmts));
}

TEST_F(InterpreterTest, InterpretBlock) {
    Scanner scanner("var x = 1; { var x = 2; print x; } print x;");
    Tokens tokens = scanner.scan();
    Parser parser(tokens);
    Stmts stmts = parser.parse();

    auto interpreter = std::make_shared<Interpreter>();
    EXPECT_NO_THROW(interpreter->interpret(stmts));
}

TEST_F(InterpreterTest, InterpretClass) {
    Scanner scanner("class Foo { bar() { print 42; } } var f = Foo(); f.bar();");
    Tokens tokens = scanner.scan();
    Parser parser(tokens);
    Stmts stmts = parser.parse();

    auto interpreter = std::make_shared<Interpreter>();
    EXPECT_NO_THROW(interpreter->interpret(stmts));
}

TEST_F(InterpreterTest, InterpretClosure) {
    Scanner scanner("fun outer() { var x = 42; fun inner() { print x; } return inner; } var f = outer(); f();");
    Tokens tokens = scanner.scan();
    Parser parser(tokens);
    Stmts stmts = parser.parse();

    auto interpreter = std::make_shared<Interpreter>();
    EXPECT_NO_THROW(interpreter->interpret(stmts));
}

} // namespace cpplox
