#include "parser.h"
#include "scanner.h"
#include <gtest/gtest.h>

namespace cpplox {

class ParserTest : public ::testing::Test {
protected:
    void SetUp() override {
        hasError_ = false;
    }
};

TEST_F(ParserTest, ParseLiteralExpression) {
    Scanner scanner("42;");
    Tokens tokens = scanner.scan();
    Parser parser(tokens);
    Stmts stmts = parser.parse();
    ASSERT_EQ(stmts.size(), 1);
}

TEST_F(ParserTest, ParseBinaryExpression) {
    Scanner scanner("1 + 2;");
    Tokens tokens = scanner.scan();
    Parser parser(tokens);
    Stmts stmts = parser.parse();
    ASSERT_EQ(stmts.size(), 1);
}

TEST_F(ParserTest, ParseUnaryExpression) {
    Scanner scanner("-42;");
    Tokens tokens = scanner.scan();
    Parser parser(tokens);
    Stmts stmts = parser.parse();
    ASSERT_EQ(stmts.size(), 1);
}

TEST_F(ParserTest, ParseGrouping) {
    Scanner scanner("(1 + 2) * 3;");
    Tokens tokens = scanner.scan();
    Parser parser(tokens);
    Stmts stmts = parser.parse();
    ASSERT_EQ(stmts.size(), 1);
}

TEST_F(ParserTest, ParseEquality) {
    Scanner scanner("1 == 2;");
    Tokens tokens = scanner.scan();
    Parser parser(tokens);
    Stmts stmts = parser.parse();
    ASSERT_EQ(stmts.size(), 1);
}

TEST_F(ParserTest, ParseComparison) {
    Scanner scanner("1 < 2;");
    Tokens tokens = scanner.scan();
    Parser parser(tokens);
    Stmts stmts = parser.parse();
    ASSERT_EQ(stmts.size(), 1);
}

TEST_F(ParserTest, ParseLogicalAnd) {
    Scanner scanner("true and false;");
    Tokens tokens = scanner.scan();
    Parser parser(tokens);
    Stmts stmts = parser.parse();
    ASSERT_EQ(stmts.size(), 1);
}

TEST_F(ParserTest, ParseLogicalOr) {
    Scanner scanner("true or false;");
    Tokens tokens = scanner.scan();
    Parser parser(tokens);
    Stmts stmts = parser.parse();
    ASSERT_EQ(stmts.size(), 1);
}

TEST_F(ParserTest, ParseAssignment) {
    Scanner scanner("a = 42;");
    Tokens tokens = scanner.scan();
    Parser parser(tokens);
    Stmts stmts = parser.parse();
    ASSERT_EQ(stmts.size(), 1);
}

TEST_F(ParserTest, ParsePrintStatement) {
    Scanner scanner("print 42;");
    Tokens tokens = scanner.scan();
    Parser parser(tokens);
    Stmts stmts = parser.parse();
    ASSERT_EQ(stmts.size(), 1);
}

TEST_F(ParserTest, ParseVarDeclaration) {
    Scanner scanner("var x = 42;");
    Tokens tokens = scanner.scan();
    Parser parser(tokens);
    Stmts stmts = parser.parse();
    ASSERT_EQ(stmts.size(), 1);
}

TEST_F(ParserTest, ParseVarDeclarationNoInit) {
    Scanner scanner("var x;");
    Tokens tokens = scanner.scan();
    Parser parser(tokens);
    Stmts stmts = parser.parse();
    ASSERT_EQ(stmts.size(), 1);
}

TEST_F(ParserTest, ParseBlock) {
    Scanner scanner("{ var x = 1; print x; }");
    Tokens tokens = scanner.scan();
    Parser parser(tokens);
    Stmts stmts = parser.parse();
    ASSERT_EQ(stmts.size(), 1);
}

TEST_F(ParserTest, ParseIfStatement) {
    Scanner scanner("if (true) print 1; else print 2;");
    Tokens tokens = scanner.scan();
    Parser parser(tokens);
    Stmts stmts = parser.parse();
    ASSERT_EQ(stmts.size(), 1);
}

TEST_F(ParserTest, ParseWhileStatement) {
    Scanner scanner("while (true) print 1;");
    Tokens tokens = scanner.scan();
    Parser parser(tokens);
    Stmts stmts = parser.parse();
    ASSERT_EQ(stmts.size(), 1);
}

TEST_F(ParserTest, ParseForStatement) {
    Scanner scanner("for (var i = 0; i < 10; i = i + 1) print i;");
    Tokens tokens = scanner.scan();
    Parser parser(tokens);
    Stmts stmts = parser.parse();
    ASSERT_EQ(stmts.size(), 1);
}

TEST_F(ParserTest, ParseFunction) {
    Scanner scanner("fun add(a, b) { return a + b; }");
    Tokens tokens = scanner.scan();
    Parser parser(tokens);
    Stmts stmts = parser.parse();
    ASSERT_EQ(stmts.size(), 1);
}

TEST_F(ParserTest, ParseReturnStatement) {
    Scanner scanner("fun f() { return 42; }");
    Tokens tokens = scanner.scan();
    Parser parser(tokens);
    Stmts stmts = parser.parse();
    ASSERT_EQ(stmts.size(), 1);
}

TEST_F(ParserTest, ParseClass) {
    Scanner scanner("class Foo { bar() { print 1; } }");
    Tokens tokens = scanner.scan();
    Parser parser(tokens);
    Stmts stmts = parser.parse();
    ASSERT_EQ(stmts.size(), 1);
}

TEST_F(ParserTest, ParseClassWithSuperclass) {
    Scanner scanner("class Foo < Bar { }");
    Tokens tokens = scanner.scan();
    Parser parser(tokens);
    Stmts stmts = parser.parse();
    ASSERT_EQ(stmts.size(), 1);
}

TEST_F(ParserTest, ParseMultipleStatements) {
    Scanner scanner("var x = 1; print x; x = 2; print x;");
    Tokens tokens = scanner.scan();
    Parser parser(tokens);
    Stmts stmts = parser.parse();
    ASSERT_EQ(stmts.size(), 4);
}

TEST_F(ParserTest, ParseErrorMissingSemicolon) {
    hasError_ = false;
    Scanner scanner("print 42");
    Tokens tokens = scanner.scan();
    Parser parser(tokens);
    Stmts stmts = parser.parse();
    EXPECT_EQ(stmts.size(), 1);
    EXPECT_EQ(stmts[0], nullptr);
    hasError_ = false;
}

TEST_F(ParserTest, ParseCallExpression) {
    Scanner scanner("foo(1, 2);");
    Tokens tokens = scanner.scan();
    Parser parser(tokens);
    Stmts stmts = parser.parse();
    ASSERT_EQ(stmts.size(), 1);
}

TEST_F(ParserTest, ParseGetExpression) {
    Scanner scanner("foo.bar;");
    Tokens tokens = scanner.scan();
    Parser parser(tokens);
    Stmts stmts = parser.parse();
    ASSERT_EQ(stmts.size(), 1);
}

TEST_F(ParserTest, ParseThisExpression) {
    Scanner scanner("this.foo;");
    Tokens tokens = scanner.scan();
    Parser parser(tokens);
    Stmts stmts = parser.parse();
    ASSERT_EQ(stmts.size(), 1);
}

TEST_F(ParserTest, ParseSuperExpression) {
    Scanner scanner("super.foo();");
    Tokens tokens = scanner.scan();
    Parser parser(tokens);
    Stmts stmts = parser.parse();
    ASSERT_EQ(stmts.size(), 1);
}

TEST_F(ParserTest, EmptyInput) {
    Scanner scanner("");
    Tokens tokens = scanner.scan();
    Parser parser(tokens);
    Stmts stmts = parser.parse();
    ASSERT_TRUE(stmts.empty());
}

} // namespace cpplox
