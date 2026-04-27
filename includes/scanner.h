#pragma once

#include "error.h"
#include "token.h"
#include "utils.h"
#include <cassert>
#include <map>
#include <string_view>

namespace cpplox {

class Scanner {
    size_t start_{0};
    size_t current_{0};
    size_t line_{1};
    std::string_view code_;
    Tokens tokens_;
    std::map<std::string, TokenType> keywords_;

    [[nodiscard]] bool isEnd() const {
        return current_ >= code_.size();
    }

    char advance() {
        assert(!isEnd());
        current_++;
        return code_[current_ - 1];
    }

    bool match(const char expect) {
        if (isEnd()) return false;
        if (code_[current_] != expect) return false;

        current_++;
        return true;
    }

    void parseString() {
        while (peek() != '"' && !isEnd()) {
            if (peek() == '\n') line_++;
            advance();
        }

        if (isEnd()) {
            error(line_, "Unterminated string.");
            return;
        }

        advance();
        const size_t size = current_ - start_ - 2;
        std::string lexeme{code_.substr(start_ + 1, size)};
        addToken(TokenType::STRING, lexeme);
    }

    [[nodiscard]] char peek() const {
        if (isEnd()) return '\0';
        return code_[current_];
    }

    [[nodiscard]] char peekNext() const {
        if (current_ + 1 >= code_.length()) return '\0';
        return code_[current_ + 1];
    }

    void number() {
        while (isDigit(peek())) advance();
        if (peek() == '.' && isDigit(peekNext())) {
            advance();
            while (isDigit(peek())) advance();
        }

        const size_t digSize = current_ - start_;
        const std::string lexeme{code_.substr(start_, digSize)};
        addToken(TokenType::NUMBER, std::stod(lexeme));
    }

    void addToken(const TokenType type) {
        addToken(type, nullptr);
    }

    void addToken(TokenType type, const std::any &literal) {
        const size_t tokenSize = current_ - start_;
        std::string text{code_.substr(start_, tokenSize)};
        tokens_.emplace_back(type, text, literal, line_);
    }

    void identifier() {
        while (isAlphaNum(peek())) advance();
        const size_t identifierSize = current_ - start_;
        const std::string identifier{code_.substr(start_, identifierSize)};
        if (keywords_.contains(identifier)) {
            addToken(keywords_[identifier]);
            return;
        }

        addToken(TokenType::IDENTIFIER);
    }

    void scanToken() {
        switch (const char c = advance()) {
            case '(': addToken(TokenType::LEFT_PAREN);
                break;
            case ')': addToken(TokenType::RIGHT_PAREN);
                break;
            case '{': addToken(TokenType::LEFT_BRACE);
                break;
            case '}': addToken(TokenType::RIGHT_BRACE);
                break;
            case ',': addToken(TokenType::COMMA);
                break;
            case '.': addToken(TokenType::DOT);
                break;
            case '-': addToken(TokenType::MINUS);
                break;
            case '+': addToken(TokenType::PLUS);
                break;
            case ';': addToken(TokenType::SEMICOLON);
                break;
            case '*': addToken(TokenType::STAR);
                break;
            case '!': addToken(match('=') ? TokenType::BANG_EQUAL : TokenType::BANG);
                break;
            case '=': addToken(match('=') ? TokenType::EQUAL_EQUAL : TokenType::EQUAL);
                break;
            case '<': addToken(match('=') ? TokenType::LESS_EQUAL : TokenType::LESS);
                break;
            case '>': addToken(match('=') ? TokenType::GREATER_EQUAL : TokenType::GREATER);
                break;
            case '"': parseString();
                break;
            case '/': {
                if (match('/')) {
                    while (peek() != '\n' && !isEnd()) advance();
                } else if (match('*')) {
                    while (!isEnd()) {
                        if (peek() == '*' && peekNext() == '/') {
                            break;
                        }
                        advance();
                    }
                    if (isEnd()) {
                        error(line_, "Unclosed multiline comment");
                    }
                    advance();
                    advance();
                } else {
                    addToken(TokenType::SLASH);
                }
            }
            break;
            case ' ':
            case '\r':
            case '\t':
                break;
            case '\n':
                line_++;
                break;
            default:
                if (isDigit(c)) {
                    number();
                } else if (isAlpha(c)) {
                    identifier();
                } else {
                    error(line_, "Unexpected character.");
                }
                break;
        }
    }

public:
    explicit Scanner(const std::string_view &code) : code_(code) {
        keywords_["and"] = TokenType::AND;
        keywords_["class"] = TokenType::CLASS;
        keywords_["else"] = TokenType::ELSE;
        keywords_["false"] = TokenType::FALSE;
        keywords_["fun"] = TokenType::FUN;
        keywords_["for"] = TokenType::FOR;
        keywords_["if"] = TokenType::IF;
        keywords_["nil"] = TokenType::NIL;
        keywords_["or"] = TokenType::OR;
        keywords_["print"] = TokenType::PRINT;
        keywords_["return"] = TokenType::RETURN;
        keywords_["super"] = TokenType::SUPER;
        keywords_["this"] = TokenType::THIS;
        keywords_["true"] = TokenType::TRUE;
        keywords_["while"] = TokenType::WHILE;
        keywords_["var"] = TokenType::VAR;
    }

    Tokens scan() {
        while (!isEnd()) {
            start_ = current_;
            scanToken();
        }

        tokens_.emplace_back(TokenType::EOFF, "", nullptr, line_);
        return tokens_;
    }
};

} // namespace cpplox
