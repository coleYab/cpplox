#pragma once

#include <vector>

namespace cpplox {

enum class TokenType {
    LEFT_PAREN = 0, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE, COMMA,
    DOT, MINUS, PLUS, SEMICOLON, SLASH, STAR,
    BANG, BANG_EQUAL, EQUAL, EQUAL_EQUAL, GREATER, GREATER_EQUAL,
    LESS, LESS_EQUAL,
    IDENTIFIER, STRING, NUMBER,
    AND, CLASS, ELSE, FALSE, FUN, FOR, IF, NIL, OR, PRINT, RETURN,
    THIS, TRUE, VAR, WHILE, SUPER,
    EOFF
};

using TokenTypes = std::vector<TokenType>;

} // namespace cpplox
