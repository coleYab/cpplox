#include "resolver.h"
#include "scanner.h"
#include "parser.h"
#include "interpreter.h"
#include <gtest/gtest.h>

namespace cpplox {

class ResolverTest : public ::testing::Test {
protected:
    void SetUp() override {
        hasError_ = false;
    }
};

TEST_F(ResolverTest, ResolveBasicExpression) {
    Scanner scanner("print 42;");
    Tokens tokens = scanner.scan();
    Parser parser(tokens);
    Stmts stmts = parser.parse();

    auto interpreter = std::make_shared<Interpreter>();
    Resolver resolver(interpreter);
    EXPECT_NO_THROW(resolver.resolve(stmts));
}

TEST_F(ResolverTest, ResolveVariable) {
    Scanner scanner("var x = 42; print x;");
    Tokens tokens = scanner.scan();
    Parser parser(tokens);
    Stmts stmts = parser.parse();

    auto interpreter = std::make_shared<Interpreter>();
    Resolver resolver(interpreter);
    EXPECT_NO_THROW(resolver.resolve(stmts));
}

TEST_F(ResolverTest, ResolveBlockScopedVariable) {
    Scanner scanner("{ var x = 42; print x; }");
    Tokens tokens = scanner.scan();
    Parser parser(tokens);
    Stmts stmts = parser.parse();

    auto interpreter = std::make_shared<Interpreter>();
    Resolver resolver(interpreter);
    EXPECT_NO_THROW(resolver.resolve(stmts));
}

TEST_F(ResolverTest, ResolveDuplicateVariableInScope) {
    hasError_ = false;
    Scanner scanner("{ var x = 1; var x = 2; }");
    Tokens tokens = scanner.scan();
    Parser parser(tokens);
    Stmts stmts = parser.parse();

    auto interpreter = std::make_shared<Interpreter>();
    Resolver resolver(interpreter);
    resolver.resolve(stmts);
    EXPECT_TRUE(hasError_);
    hasError_ = false;
}

TEST_F(ResolverTest, ResolveFunction) {
    Scanner scanner("fun f() { print 42; } f();");
    Tokens tokens = scanner.scan();
    Parser parser(tokens);
    Stmts stmts = parser.parse();

    auto interpreter = std::make_shared<Interpreter>();
    Resolver resolver(interpreter);
    EXPECT_NO_THROW(resolver.resolve(stmts));
}

TEST_F(ResolverTest, ResolveClass) {
    Scanner scanner("class Foo { bar() { print 42; } }");
    Tokens tokens = scanner.scan();
    Parser parser(tokens);
    Stmts stmts = parser.parse();

    auto interpreter = std::make_shared<Interpreter>();
    Resolver resolver(interpreter);
    EXPECT_NO_THROW(resolver.resolve(stmts));
}

TEST_F(ResolverTest, ResolveThisOutsideClass) {
    hasError_ = false;
    Scanner scanner("print this;");
    Tokens tokens = scanner.scan();
    Parser parser(tokens);
    Stmts stmts = parser.parse();

    auto interpreter = std::make_shared<Interpreter>();
    Resolver resolver(interpreter);
    resolver.resolve(stmts);
    EXPECT_TRUE(hasError_);
    hasError_ = false;
}

TEST_F(ResolverTest, ResolveThisInsideClass) {
    hasError_ = false;
    Scanner scanner("class Foo { bar() { print this; } }");
    Tokens tokens = scanner.scan();
    Parser parser(tokens);
    Stmts stmts = parser.parse();

    auto interpreter = std::make_shared<Interpreter>();
    Resolver resolver(interpreter);
    resolver.resolve(stmts);
    EXPECT_FALSE(hasError_);
}

TEST_F(ResolverTest, ResolveSuperOutsideClass) {
    hasError_ = false;
    Scanner scanner("print super.foo;");
    Tokens tokens = scanner.scan();
    Parser parser(tokens);
    Stmts stmts = parser.parse();

    auto interpreter = std::make_shared<Interpreter>();
    Resolver resolver(interpreter);
    resolver.resolve(stmts);
    EXPECT_TRUE(hasError_);
    hasError_ = false;
}

TEST_F(ResolverTest, ResolveReturnOutsideFunction) {
    hasError_ = false;
    Scanner scanner("return 42;");
    Tokens tokens = scanner.scan();
    Parser parser(tokens);
    Stmts stmts = parser.parse();

    auto interpreter = std::make_shared<Interpreter>();
    Resolver resolver(interpreter);
    resolver.resolve(stmts);
    EXPECT_TRUE(hasError_);
    hasError_ = false;
}

TEST_F(ResolverTest, ResolveReturnInsideFunction) {
    hasError_ = false;
    Scanner scanner("fun f() { return 42; }");
    Tokens tokens = scanner.scan();
    Parser parser(tokens);
    Stmts stmts = parser.parse();

    auto interpreter = std::make_shared<Interpreter>();
    Resolver resolver(interpreter);
    resolver.resolve(stmts);
    EXPECT_FALSE(hasError_);
}

TEST_F(ResolverTest, ResolveInitializerReturn) {
    hasError_ = false;
    Scanner scanner("class Foo { init() { return 42; } }");
    Tokens tokens = scanner.scan();
    Parser parser(tokens);
    Stmts stmts = parser.parse();

    auto interpreter = std::make_shared<Interpreter>();
    Resolver resolver(interpreter);
    resolver.resolve(stmts);
    EXPECT_TRUE(hasError_);
    hasError_ = false;
}

TEST_F(ResolverTest, ResolveIfAndWhile) {
    Scanner scanner("if (true) { var x = 1; } while (false) { var y = 2; }");
    Tokens tokens = scanner.scan();
    Parser parser(tokens);
    Stmts stmts = parser.parse();

    auto interpreter = std::make_shared<Interpreter>();
    Resolver resolver(interpreter);
    EXPECT_NO_THROW(resolver.resolve(stmts));
}

TEST_F(ResolverTest, ResolveFullProgram) {
    Scanner scanner(
        "var x = 10;\n"
        "fun add(a, b) {\n"
        "  return a + b;\n"
        "}\n"
        "class Counter {\n"
        "  init(value) {\n"
        "    this.val = value;\n"
        "  }\n"
        "  inc() {\n"
        "    this.val = this.val + 1;\n"
        "  }\n"
        "}\n"
        "print add(1, 2);\n"
    );
    Tokens tokens = scanner.scan();
    Parser parser(tokens);
    Stmts stmts = parser.parse();

    auto interpreter = std::make_shared<Interpreter>();
    Resolver resolver(interpreter);
    EXPECT_NO_THROW(resolver.resolve(stmts));
}

} // namespace cpplox
