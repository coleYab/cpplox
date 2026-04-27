#pragma once

#include <any>
#include <cassert>
#include <string>

namespace cpplox {

class Helper {
public:
    static bool isNumber(const std::any &x) noexcept {
        return x.type() == typeid(int) || x.type() == typeid(double);
    }

    static bool isString(const std::any &x) noexcept {
        return x.type() == typeid(std::string);
    }

    static bool isBoolean(const std::any &x) {
        return x.type() == typeid(bool);
    }

    static bool isNull(const std::any &x) {
        return x.type() == typeid(std::nullptr_t);
    }

    static double getNumber(const std::any &x) noexcept {
        assert(isNumber(x));
        try {
            return std::any_cast<double>(x);
        } catch (...) {
            return 0;
        }
    }

    static std::string getString(const std::any &x) noexcept {
        assert(isString(x));
        try {
            return std::any_cast<std::string>(x);
        } catch (...) {
            return "";
        }
    }

    static bool getBoolean(const std::any &x) {
        assert(isBoolean(x));
        try {
            return std::any_cast<bool>(x);
        } catch (...) {
            return false;
        }
    }

    static bool isTruthy(const std::any &obj) noexcept {
        if (!obj.has_value()) return false;
        if (obj.type() == typeid(bool)) return std::any_cast<bool>(obj);
        if (obj.type() == typeid(double)) return std::any_cast<double>(obj) != 0.0;
        if (obj.type() == typeid(std::string)) return !std::any_cast<std::string>(obj).empty();
        return true;
    }

    static bool isSameType(const std::any &left, const std::any &right) noexcept {
        return left.type() == right.type();
    }

    static bool isEqual(const std::any &left, const std::any &right) {
        if (!isSameType(left, right)) return false;
        if (isNumber(left)) return getNumber(left) == getNumber(right);
        if (isString(left)) return getString(left) == getString(right);
        if (isBoolean(left)) return getBoolean(left) == getBoolean(right);
        return false;
    }

    static bool isGreater(const std::any &left, const std::any &right) {
        if (!isSameType(left, right)) return false;
        if (isNumber(left)) return getNumber(left) > getNumber(right);
        if (isString(left)) return getString(left) > getString(right);
        return false;
    }

    static bool isLess(const std::any &left, const std::any &right) {
        if (!isSameType(left, right) || isEqual(left, right)) return false;
        return !isGreater(left, right);
    }

    static bool isGreaterEqual(const std::any &left, const std::any &right) {
        return isGreater(left, right) || isEqual(left, right);
    }

    static bool isLessEqual(const std::any &left, const std::any &right) {
        return isLess(left, right) || isEqual(left, right);
    }

    static bool isAddSupported(const std::any &left, const std::any &right) {
        if (!isSameType(left, right)) return false;
        return isNumber(left) || isString(left);
    }

    static bool isStarSupported(const std::any &left, const std::any &right) {
        return isSameType(left, right) && isNumber(left);
    }

    static bool isCompSupported(const std::any &left, const std::any &right) {
        if (!isSameType(left, right)) return false;
        return isNumber(left) || isBoolean(left) || isString(left);
    }

    static bool isSlashSupported(const std::any &left, const std::any &right) {
        if (!isSameType(left, right)) return false;
        return isNumber(left);
    }

    static std::string toString(const std::any &x) {
        if (isNumber(x)) return std::to_string(getNumber(x));
        if (isBoolean(x)) return getBoolean(x) ? "true" : "false";
        if (isString(x)) return getString(x);
        if (isNull(x)) return "nil";
        return "?";
    }
};

} // namespace cpplox
