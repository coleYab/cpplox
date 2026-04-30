#include "environment.h"
#include <gtest/gtest.h>

namespace cpplox {

TEST(EnvironmentTest, DefineAndGet) {
    auto env = std::make_shared<Environment>();
    env->define("x", 42.0);
    auto val = env->get("x");
    EXPECT_DOUBLE_EQ(std::any_cast<double>(val), 42.0);
}

TEST(EnvironmentTest, GetThrowsForUndefined) {
    auto env = std::make_shared<Environment>();
    EXPECT_THROW((void)env->get("undefined"), std::runtime_error);
}

TEST(EnvironmentTest, DefineThrowsForDuplicate) {
    auto env = std::make_shared<Environment>();
    env->define("x", 1.0);
    EXPECT_THROW(env->define("x", 2.0), std::runtime_error);
}

TEST(EnvironmentTest, DefineIfSilentlyIgnoresDuplicate) {
    auto env = std::make_shared<Environment>();
    env->defineIf("x", 1.0);
    EXPECT_NO_THROW(env->defineIf("x", 2.0));
}

TEST(EnvironmentTest, AssignExisting) {
    auto env = std::make_shared<Environment>();
    env->define("x", 1.0);
    env->assign("x", 99.0);
    auto val = env->get("x");
    EXPECT_DOUBLE_EQ(std::any_cast<double>(val), 99.0);
}

TEST(EnvironmentTest, AssignThrowsForUndefined) {
    auto env = std::make_shared<Environment>();
    EXPECT_THROW(env->assign("x", 1.0), std::runtime_error);
}

TEST(EnvironmentTest, NestedEnvironmentGetFromEnclosing) {
    auto global = std::make_shared<Environment>();
    global->define("x", 10.0);

    auto local = std::make_shared<Environment>(global);
    auto val = local->get("x");
    EXPECT_DOUBLE_EQ(std::any_cast<double>(val), 10.0);
}

TEST(EnvironmentTest, NestedEnvironmentShadowsEnclosing) {
    auto global = std::make_shared<Environment>();
    global->define("x", 10.0);

    auto local = std::make_shared<Environment>(global);
    local->define("x", 20.0);
    EXPECT_DOUBLE_EQ(std::any_cast<double>(local->get("x")), 20.0);
    EXPECT_DOUBLE_EQ(std::any_cast<double>(global->get("x")), 10.0);
}

TEST(EnvironmentTest, AssignToEnclosingScope) {
    auto global = std::make_shared<Environment>();
    global->define("x", 10.0);

    auto local = std::make_shared<Environment>(global);
    local->assign("x", 99.0);
    EXPECT_DOUBLE_EQ(std::any_cast<double>(global->get("x")), 99.0);
}

TEST(EnvironmentTest, GetAtDistance) {
    auto global = std::make_shared<Environment>();
    global->define("x", 10.0);

    auto mid = std::make_shared<Environment>(global);
    mid->define("x", 20.0);

    auto local = std::make_shared<Environment>(mid);
    local->define("x", 30.0);

    EXPECT_DOUBLE_EQ(std::any_cast<double>(local->getAt(0, "x")), 30.0);
    EXPECT_DOUBLE_EQ(std::any_cast<double>(local->getAt(1, "x")), 20.0);
    EXPECT_DOUBLE_EQ(std::any_cast<double>(local->getAt(2, "x")), 10.0);
}

TEST(EnvironmentTest, AssignAtDistance) {
    auto global = std::make_shared<Environment>();
    global->define("x", 10.0);

    auto mid = std::make_shared<Environment>(global);
    mid->define("x", 20.0);

    auto local = std::make_shared<Environment>(mid);

    local->assignAt(1, "x", 99.0);
    EXPECT_DOUBLE_EQ(std::any_cast<double>(mid->get("x")), 99.0);
    EXPECT_DOUBLE_EQ(std::any_cast<double>(global->get("x")), 10.0);
}

TEST(EnvironmentTest, Clear) {
    auto env = std::make_shared<Environment>();
    env->define("x", 1.0);
    env->clear();
    EXPECT_THROW((void)env->get("x"), std::runtime_error);
}

} // namespace cpplox
