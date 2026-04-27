#pragma once

#include "token_type.h"
#include <any>
#include <string>
#include <vector>

namespace cpplox {

class Token {
    TokenType type_;
    std::string lexeme_;
    std::any literal_;
    size_t line_;

public:
    Token(const TokenType type, std::string lexeme, std::any literal, const size_t line)
        : type_(type), lexeme_(std::move(lexeme)), literal_(std::move(literal)), line_(line) {}

    [[nodiscard]] TokenType getType() const { return type_; }
    [[nodiscard]] const std::any &getLiteral() const { return literal_; }
    [[nodiscard]] const std::string &getLexeme() const { return lexeme_; }
    [[nodiscard]] size_t getLine() const { return line_; }

    [[nodiscard]] std::string toString() const {
        return std::to_string(static_cast<int>(type_)) + " " + lexeme_ + " " + std::to_string(line_);
    }
};

using Tokens = std::vector<Token>;

} // namespace cpplox
