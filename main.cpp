#include <iostream>
#include <map>
#include <any>
#include <vector>
#include <cassert>
#include <fstream>
#include <filesystem>
#include <string_view>

namespace cpplox {
    // TODO: find some way to remove this hasError_ or abstract it away in some class cpplox
    bool hasError_ { false };
    void report(const int line, const std::string &where, const std::string &message) {
        std::cerr << "[" << line << "] Error " << where << ": " << message << std::endl;
    }

    void error(const int line, const std::string &message) {
        report(line, "", message);
        hasError_ = true;
    }

    enum class TokenType {
        // Single character tokens.
        LEFT_PAREN = 0, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE, COMMA,
        DOT, MINUS, PLUS, SEMICOLON, SLASH, STAR,

        // one or two character tokens.
        BANG, BANG_EQUAL, EQUAL, EQUAL_EQUAL, GREATER, GREATER_EQUAL,
        LESS, LESS_EQUAL,

        // literals
        identifier, string, number,

        // keywords
        AND, CLASS, ELSE, FALSE, FUN, FOR, IF, NIL, OR, PRINT, RETURN,
        THIS, TRUE, VAR, WHILE, SUPER,

        EOFF
    };

    class Token {
        TokenType type_;
        std::string lexeme_;
        std::any literal_;
        int line_;

    public:
        Token(const TokenType type, std::string lexeme, std::any literal, const int line) :
            type_(type), lexeme_(std::move(lexeme)), literal_(std::move(literal)), line_(line) {}

        [[nodiscard]] TokenType getType() const {
            return type_;
        }

        [[nodiscard]] const std::any &getLiteral() const {
            return literal_;
        }

        [[nodiscard]] const std::string &getLexeme() const {
            return lexeme_;
        }

        [[nodiscard]] int getLine() const {
            return line_;
        }

        [[nodiscard]] std::string toString() const {
            return std::to_string(static_cast<int>(type_)) + " " + lexeme_ + " " + std::to_string(line_);
        }
    };

    using Tokens = std::vector<Token>;

    class Scanner {
        int start_ {0};
        int current_ { 0 };
        int line_ { 1 };
        std::string_view code_;
        Tokens tokens_;

        std::map<std::string, TokenType> keywords_;

        bool isEnd() const {
            return current_ >= static_cast<int>(code_.size());
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
            std::string lexeme { code_.substr(start_ + 1, current_ - 1)};
            addToken(TokenType::string, lexeme);
        }

        bool isDigit(const char c) const {
            return '0' <= c && c <= '9';
        }

        char peek() const {
            if (isEnd()) return '\0';
            return code_[current_];
        }

        char peekNext() const {
            if (current_ + 1 >= static_cast<int>(code_.length())) return '\0';
            return code_[current_ + 1];
        }

        void number() {
            while (isDigit(peek())) advance();
            if (peek() == '.' && isDigit(peekNext())) {
                advance();
                while (isDigit(peek())) advance();
            }

            const int digSize = current_ - start_;
            std::string lexeme { code_.substr(start_, digSize)};
            addToken(TokenType::number, lexeme);
        }

        void addToken(const TokenType type) {
            addToken(type, nullptr);
        }

        void addToken(TokenType type, const std::any& literal) {
            const int tokenSize = current_ - start_;
            std::string text { code_.substr(start_, tokenSize) };
            tokens_.emplace_back(type, text, literal, line_);
        }

       [[nodiscard]] bool isAlpha(const char c) const {
            const bool isLower = 'a' <= c && c <= 'z';
            const bool isUpper = 'A' <= c && c <= 'Z';
            return isLower || isUpper || c == '_';
        }

        [[nodiscard]] bool isAlphaNum(const char c) const {
            return isDigit(c) || isAlpha(c);
        }

        void identifier() {
            while (isAlphaNum(peek())) advance();
            const int identifierSize = current_ - start_;
            const std::string identifier {code_.substr(start_, identifierSize )};
            if (keywords_.find(identifier) != keywords_.end()) {
                addToken(keywords_[identifier]);
                return;
            }

            addToken(TokenType::identifier);
        }

        void scanToken() {
            char c = advance();
            switch (c) {
                case '(': addToken(TokenType::LEFT_PAREN); break;
                case ')': addToken(TokenType::RIGHT_PAREN); break;
                case '{': addToken(TokenType::LEFT_BRACE); break;
                case '}': addToken(TokenType::RIGHT_BRACE); break;
                case ',': addToken(TokenType::COMMA); break;
                case '.': addToken(TokenType::DOT); break;
                case '-': addToken(TokenType::MINUS); break;
                case '+': addToken(TokenType::PLUS); break;
                case ';': addToken(TokenType::SEMICOLON); break;
                case '*': addToken(TokenType::STAR); break;
                case '!': addToken(match('=') ? TokenType::BANG_EQUAL : TokenType::BANG); break;
                case '=': addToken(match('=') ? TokenType::EQUAL_EQUAL : TokenType::EQUAL); break;
                case '<': addToken(match('=') ? TokenType::LESS_EQUAL : TokenType::LESS); break;
                case '>': addToken(match('=') ? TokenType::GREATER_EQUAL : TokenType::GREATER); break;
                case '/': {
                    if (match('/')) {
                        while(peek() != '\n' && !isEnd()) advance();
                    } else if (match('*')) {
                        while (!isEnd()) {
                            bool commentEnd = peek() == '*' && peekNext() == '/';
                            if (commentEnd) {
                                break;
                            }

                            advance();
                        }
                        if (isEnd()) {
                            error(line_, "Unclosed multiline comment");
                        }

                        // consume the comment ending tokens
                        advance();
                        advance();
                    } else {
                        addToken(TokenType::SLASH);
                    }
                } break;
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

    void run(const std::string_view &code) {
        Scanner scanner { code };
        const Tokens tokens { scanner.scan() };
        if (hasError_) {
            exit(2);
        }
        for (const auto &token : tokens) std::cout << token.toString() << std::endl;
    }

    void runFile(const std::string& fileName) {
        std::ifstream infile { fileName };

        if (!infile.is_open()) {
            throw std::runtime_error("could not open file " + fileName );
        }

        // TODO: rather than loading all the fileContent make sure to parse the source code line by line
        const std::string sCode { std::istreambuf_iterator<char>(infile), std::istreambuf_iterator<char>() };
        run(sCode);
        hasError_ = false;
    }

    void runPrompt() {
        while (true) {
            std::cout << ">>> ";
            std::string prompt;
            std::getline(std::cin, prompt);
            if (prompt.empty()) break;
            run(prompt);
        }
    }
}; // end namespace cpplox;

int main(const int argv, char **argc) {
    assert(argv >= 1);

    if (argv == 2) {
        const std::string fileName{argc[1]};
        cpplox::runFile(fileName);
    } else if (argv == 1) {
        cpplox::runPrompt();
    } else {
        std::cout << "Usage cpplox [script]" << std::endl;
        exit(1);
    }
}