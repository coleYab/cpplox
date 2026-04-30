#include "utils.h"
#include <gtest/gtest.h>

namespace cpplox {

TEST(UtilsTest, isDigit_ReturnsTrueForDigits) {
    for (char c = '0'; c <= '9'; c++) {
        EXPECT_TRUE(isDigit(c));
    }
}

TEST(UtilsTest, isDigit_ReturnsFalseForNonDigits) {
    EXPECT_FALSE(isDigit('a'));
    EXPECT_FALSE(isDigit('Z'));
    EXPECT_FALSE(isDigit('_'));
    EXPECT_FALSE(isDigit(' '));
    EXPECT_FALSE(isDigit('\0'));
}

TEST(UtilsTest, isAlpha_ReturnsTrueForLetters) {
    for (char c = 'a'; c <= 'z'; c++) {
        EXPECT_TRUE(isAlpha(c));
    }
    for (char c = 'A'; c <= 'Z'; c++) {
        EXPECT_TRUE(isAlpha(c));
    }
}

TEST(UtilsTest, isAlpha_ReturnsTrueForUnderscore) {
    EXPECT_TRUE(isAlpha('_'));
}

TEST(UtilsTest, isAlpha_ReturnsFalseForNonAlpha) {
    EXPECT_FALSE(isAlpha('0'));
    EXPECT_FALSE(isAlpha('9'));
    EXPECT_FALSE(isAlpha(' '));
    EXPECT_FALSE(isAlpha('.'));
}

TEST(UtilsTest, isAlphaNum_ReturnsTrueForAlphanumeric) {
    for (char c = '0'; c <= '9'; c++) {
        EXPECT_TRUE(isAlphaNum(c));
    }
    for (char c = 'a'; c <= 'z'; c++) {
        EXPECT_TRUE(isAlphaNum(c));
    }
    for (char c = 'A'; c <= 'Z'; c++) {
        EXPECT_TRUE(isAlphaNum(c));
    }
}

TEST(UtilsTest, isAlphaNum_ReturnsTrueForUnderscore) {
    EXPECT_TRUE(isAlphaNum('_'));
}

TEST(UtilsTest, isAlphaNum_ReturnsFalseForNonAlphaNum) {
    EXPECT_FALSE(isAlphaNum(' '));
    EXPECT_FALSE(isAlphaNum('.'));
    EXPECT_FALSE(isAlphaNum('\0'));
}

} // namespace cpplox
