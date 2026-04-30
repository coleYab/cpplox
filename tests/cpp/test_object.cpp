#include "object.h"
#include "scanner.h"
#include "parser.h"
#include "interpreter.h"
#include <gtest/gtest.h>

namespace cpplox {

TEST(ObjectTest, LoxClassConstructor) {
    Token name(TokenType::IDENTIFIER, "Foo", nullptr, 1);
    auto klass = std::make_shared<LoxClass>("Foo", nullptr, std::map<std::string, std::shared_ptr<LoxFunction>>{});
    EXPECT_EQ(klass->getName(), "Foo");
    EXPECT_EQ(klass->arity(), 0);
    EXPECT_EQ(klass->findMethod("nonexistent"), nullptr);
}

TEST(ObjectTest, LoxClassWithSuperclass) {
    auto super = std::make_shared<LoxClass>("Super", nullptr, std::map<std::string, std::shared_ptr<LoxFunction>>{});
    auto sub = std::make_shared<LoxClass>("Sub", super, std::map<std::string, std::shared_ptr<LoxFunction>>{});
    EXPECT_EQ(sub->getName(), "Sub");
    EXPECT_EQ(super->getName(), "Super");
}

TEST(ObjectTest, LoxInstanceToString) {
    auto klass = std::make_shared<LoxClass>("Foo", nullptr, std::map<std::string, std::shared_ptr<LoxFunction>>{});
    auto instance = std::make_shared<LoxInstance>(klass);
    EXPECT_EQ(instance->toString(), "Foo instance");
}

TEST(ObjectTest, LoxInstanceSetAndGetField) {
    auto klass = std::make_shared<LoxClass>("Foo", nullptr, std::map<std::string, std::shared_ptr<LoxFunction>>{});
    auto instance = std::make_shared<LoxInstance>(klass);
    Token name(TokenType::IDENTIFIER, "x", nullptr, 1);
    instance->set(name, 42.0);
    auto val = instance->get(name);
    EXPECT_DOUBLE_EQ(std::any_cast<double>(val), 42.0);
}

TEST(ObjectTest, LoxInstanceGetThrowsOnMissingField) {
    auto klass = std::make_shared<LoxClass>("Foo", nullptr, std::map<std::string, std::shared_ptr<LoxFunction>>{});
    auto instance = std::make_shared<LoxInstance>(klass);
    Token name(TokenType::IDENTIFIER, "x", nullptr, 1);
    EXPECT_THROW(instance->get(name), RuntimeError);
}

TEST(ObjectTest, LoxClassFindMethod) {
    Token name(TokenType::IDENTIFIER, "Foo", nullptr, 1);
    Token param(TokenType::IDENTIFIER, "x", nullptr, 1);
    std::vector<Token> params{param};

    Scanner scanner("print x;");
    Tokens tokens = scanner.scan();
    Parser parser(tokens);
    Stmts body = parser.parse();

    auto decl = std::make_shared<Function>(name, params, body);
    auto closure = std::make_shared<Environment>();
    auto func = std::make_shared<LoxFunction>(decl, closure);

    std::map<std::string, std::shared_ptr<LoxFunction>> methods;
    methods["bar"] = func;

    auto klass = std::make_shared<LoxClass>("Foo", nullptr, methods);
    auto found = klass->findMethod("bar");
    EXPECT_NE(found, nullptr);
    EXPECT_EQ(klass->findMethod("nope"), nullptr);
}

TEST(ObjectTest, NativeFunction) {
    NativeFunction nf(0, [](Interpreter &, const std::vector<std::any> &) -> std::any {
        return 42.0;
    });
    EXPECT_EQ(nf.arity(), 0);
}

TEST(ObjectTest, ReturnException) {
    ReturnException ex(42.0);
    EXPECT_DOUBLE_EQ(std::any_cast<double>(ex.value_), 42.0);
}

} // namespace cpplox
