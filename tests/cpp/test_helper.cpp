#include "helper.h"
#include <gtest/gtest.h>

namespace cpplox {

TEST(HelperTest, isNumber_IntOrDouble) {
    EXPECT_TRUE(Helper::isNumber(std::any(42.0)));
    EXPECT_FALSE(Helper::isNumber(std::any("hello")));
    EXPECT_FALSE(Helper::isNumber(std::any(true)));
    EXPECT_FALSE(Helper::isNumber(std::any(nullptr)));
}

TEST(HelperTest, isString) {
    EXPECT_TRUE(Helper::isString(std::any(std::string("hello"))));
    EXPECT_FALSE(Helper::isString(std::any(42.0)));
}

TEST(HelperTest, isBoolean) {
    EXPECT_TRUE(Helper::isBoolean(std::any(true)));
    EXPECT_TRUE(Helper::isBoolean(std::any(false)));
    EXPECT_FALSE(Helper::isBoolean(std::any(42.0)));
}

TEST(HelperTest, isNull) {
    EXPECT_TRUE(Helper::isNull(std::any(nullptr)));
    EXPECT_FALSE(Helper::isNull(std::any(42.0)));
}

TEST(HelperTest, getNumber) {
    EXPECT_DOUBLE_EQ(Helper::getNumber(std::any(42.5)), 42.5);
    EXPECT_DOUBLE_EQ(Helper::getNumber(std::any(0.0)), 0.0);
}

TEST(HelperTest, getString) {
    EXPECT_EQ(Helper::getString(std::any(std::string("hi"))), "hi");
    EXPECT_EQ(Helper::getString(std::any(std::string(""))), "");
}

TEST(HelperTest, getBoolean) {
    EXPECT_TRUE(Helper::getBoolean(std::any(true)));
    EXPECT_FALSE(Helper::getBoolean(std::any(false)));
}

TEST(HelperTest, isTruthy) {
    EXPECT_FALSE(Helper::isTruthy(std::any{}));
    EXPECT_TRUE(Helper::isTruthy(std::any(42.0)));
    EXPECT_FALSE(Helper::isTruthy(std::any(0.0)));
    EXPECT_TRUE(Helper::isTruthy(std::any(std::string("hi"))));
    EXPECT_FALSE(Helper::isTruthy(std::any(std::string(""))));
    EXPECT_TRUE(Helper::isTruthy(std::any(true)));
    EXPECT_FALSE(Helper::isTruthy(std::any(false)));
}

TEST(HelperTest, isSameType) {
    EXPECT_TRUE(Helper::isSameType(std::any(1.0), std::any(2.0)));
    EXPECT_FALSE(Helper::isSameType(std::any(1.0), std::any("hi")));
}

TEST(HelperTest, isEqual) {
    EXPECT_TRUE(Helper::isEqual(std::any(1.0), std::any(1.0)));
    EXPECT_FALSE(Helper::isEqual(std::any(1.0), std::any(2.0)));
    EXPECT_TRUE(Helper::isEqual(std::any(std::string("a")), std::any(std::string("a"))));
    EXPECT_FALSE(Helper::isEqual(std::any(std::string("a")), std::any(std::string("b"))));
    EXPECT_TRUE(Helper::isEqual(std::any(true), std::any(true)));
    EXPECT_FALSE(Helper::isEqual(std::any(true), std::any(false)));
    EXPECT_FALSE(Helper::isEqual(std::any(1.0), std::any(true)));
}

TEST(HelperTest, isGreaterAndLess) {
    EXPECT_TRUE(Helper::isGreater(std::any(2.0), std::any(1.0)));
    EXPECT_FALSE(Helper::isGreater(std::any(1.0), std::any(2.0)));
    EXPECT_FALSE(Helper::isGreater(std::any(1.0), std::any(1.0)));
    EXPECT_TRUE(Helper::isLess(std::any(1.0), std::any(2.0)));
    EXPECT_FALSE(Helper::isLess(std::any(2.0), std::any(1.0)));
    EXPECT_FALSE(Helper::isLess(std::any(1.0), std::any(1.0)));
}

TEST(HelperTest, isGreaterEqualAndLessEqual) {
    EXPECT_TRUE(Helper::isGreaterEqual(std::any(2.0), std::any(1.0)));
    EXPECT_TRUE(Helper::isGreaterEqual(std::any(1.0), std::any(1.0)));
    EXPECT_FALSE(Helper::isGreaterEqual(std::any(0.0), std::any(1.0)));
    EXPECT_TRUE(Helper::isLessEqual(std::any(1.0), std::any(2.0)));
    EXPECT_TRUE(Helper::isLessEqual(std::any(1.0), std::any(1.0)));
    EXPECT_FALSE(Helper::isLessEqual(std::any(2.0), std::any(1.0)));
}

TEST(HelperTest, isAddSupported) {
    EXPECT_TRUE(Helper::isAddSupported(std::any(1.0), std::any(2.0)));
    EXPECT_TRUE(Helper::isAddSupported(std::any(std::string("a")), std::any(std::string("b"))));
    EXPECT_FALSE(Helper::isAddSupported(std::any(1.0), std::any(std::string("b"))));
    EXPECT_FALSE(Helper::isAddSupported(std::any(true), std::any(false)));
}

TEST(HelperTest, isStarSupported) {
    EXPECT_TRUE(Helper::isStarSupported(std::any(2.0), std::any(3.0)));
    EXPECT_FALSE(Helper::isStarSupported(std::any(2.0), std::any(std::string("a"))));
}

TEST(HelperTest, isCompSupported) {
    EXPECT_TRUE(Helper::isCompSupported(std::any(1.0), std::any(2.0)));
    EXPECT_TRUE(Helper::isCompSupported(std::any(true), std::any(false)));
    EXPECT_TRUE(Helper::isCompSupported(std::any(std::string("a")), std::any(std::string("b"))));
    EXPECT_FALSE(Helper::isCompSupported(std::any(1.0), std::any(nullptr)));
}

TEST(HelperTest, toString) {
    EXPECT_EQ(Helper::toString(std::any(42.5)), "42.500000");
    EXPECT_EQ(Helper::toString(std::any(true)), "true");
    EXPECT_EQ(Helper::toString(std::any(false)), "false");
    EXPECT_EQ(Helper::toString(std::any(std::string("hi"))), "hi");
    EXPECT_EQ(Helper::toString(std::any(nullptr)), "nil");
}

} // namespace cpplox
