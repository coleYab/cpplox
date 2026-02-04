#include "includes/utils.h"

namespace cpplox {
    [[nodiscard]] bool isDigit(const char c) {
        return '0' <= c && c <= '9';
    }

    [[nodiscard]] bool isAlpha(const char c) {
        const bool isUpper = 'A' <= c && c <= 'Z';
        const bool isLower = 'a' <= c && c <= 'z';
        return isUpper || isLower || c == '_';
    }

    [[nodiscard]] bool isAlphaNum(const char c) {
        return isDigit(c) || isAlpha(c);
    }
}
