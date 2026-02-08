#include <algorithm>
#include <iostream>
#include <map>
#include <any>
#include <utility>
#include <vector>
#include <cassert>
#include <complex>
#include <fstream>
#include <filesystem>
#include <string_view>
#include "includes/utils.h"
#include <functional>

namespace cpplox {
    // TODO: find some way to remove this hasError_ or abstract it away in some class cpplox
    bool hasError_{false};

    void report(const int line, const std::string &where, const std::string &message) {
        std::cerr << "[" << line << "] Error " << where << ": " << message << std::endl;
    }

    // TODO: throw these errors rather than maintaining this thing
    class RuntimeError : public std::exception {
    };

    class CompilationError : public std::exception {
    };

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
        IDENTIFIER, STRING, NUMBER,

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
        Token(const TokenType type, std::string lexeme, std::any literal, const int line) : type_(type),
            lexeme_(std::move(lexeme)), literal_(std::move(literal)), line_(line) {
        }

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


    class Binary;
    class Grouping;
    class Literal;
    class Unary;
    class Variable;
    class Assign;
    class Logical;
    class Call;
    class Return;

    class Visitor {
    public:
        virtual ~Visitor() = default;

        virtual std::any visitBinaryExpr(Binary &E) = 0;

        virtual std::any visitGroupingExpr(Grouping &E) = 0;

        virtual std::any visitLiteralExpr(Literal &E) = 0;

        virtual std::any visitUnaryExpr(Unary &E) = 0;

        virtual std::any visitVariableExpr(Variable &E) = 0;

        virtual std::any visitAssignExpr(Assign &E) = 0;

        virtual std::any visitLogicalExpr(Logical &E) = 0;

        virtual std::any visitCallExpr(Call &E) = 0;
    };

    class Expr {
    public:
        virtual ~Expr() = default;

        virtual std::any accept(Visitor &visitor) = 0;
    };


    class Binary : public Expr {
    public:
        Binary(std::shared_ptr<Expr> left, Token op, std::shared_ptr<Expr> right) : left(std::move(left)),
            op(std::move(op)), right(std::move(right)) {
        }

        std::any accept(Visitor &visitor) override {
            return visitor.visitBinaryExpr(*this);
        }

        const std::shared_ptr<Expr> left;
        const Token op;
        const std::shared_ptr<Expr> right;
    };

    class Call : public Expr {
    public:
        std::shared_ptr<Expr> callee;
        Token par;
        std::vector<std::shared_ptr<Expr> > arguments;

        Call(std::shared_ptr<Expr> callee, Token par,
             std::vector<std::shared_ptr<Expr> > arguments) : callee(std::move(callee)), par(std::move(par)),
                                                              arguments(std::move(arguments)) {
        }

        std::any accept(Visitor &visitor) override {
            return visitor.visitCallExpr(*this);
        }
    };


    class Grouping : public Expr {
    public:
        explicit Grouping(std::shared_ptr<Expr> expression) : expression(std::move(expression)) {
        }

        std::any accept(Visitor &visitor) override {
            return visitor.visitGroupingExpr(*this);
        }

        const std::shared_ptr<Expr> expression;
    };

    class Literal : public Expr {
    public:
        explicit Literal(std::any value) : value(std::move(value)) {
        }

        std::any accept(Visitor &visitor) override {
            return visitor.visitLiteralExpr(*this);
        }

        const std::any value;
    };

    class Logical : public Expr {
    public:
        std::shared_ptr<Expr> left;
        Token op;
        std::shared_ptr<Expr> right;

        Logical(std::shared_ptr<Expr> left, Token op, std::shared_ptr<Expr> right) : left(std::move(left)),
            op(std::move(op)), right(std::move(right)) {
        }

        std::any accept(Visitor &visitor) override {
            return visitor.visitLogicalExpr(*this);
        }
    };

    class Unary : public Expr {
    public:
        Unary(Token op, std::shared_ptr<Expr> right) : op(std::move(op)), right(std::move(right)) {
        }

        std::any accept(Visitor &visitor) override {
            return visitor.visitUnaryExpr(*this);
        }

        const Token op;
        const std::shared_ptr<Expr> right;
    };

    class Variable : public Expr {
    public:
        Token name;

        explicit Variable(Token name) : name(std::move(name)) {
        }

        std::any accept(Visitor &visitor) override {
            return visitor.visitVariableExpr(*this);
        }
    };

    class Assign : public Expr {
    public:
        Token name;
        std::shared_ptr<Expr> value;

        Assign(Token name, std::shared_ptr<Expr> value) : name(std::move(name)), value(std::move(value)) {
        }

        std::any accept(Visitor &visitor) override {
            return visitor.visitAssignExpr(*this);
        }
    };

    // Define the classes ahead
    class Stmt;
    class Expression;
    class Print;
    class Var;
    class Block;
    class If;
    class While;
    class Function;

    class Stmt {
    public:
        class Visitor {
        public:
            virtual ~Visitor() = default;

            virtual std::any visitSExpressionStmt(const Expression &) = 0;

            virtual std::any visitPrintStmt(const Print &) = 0;

            virtual std::any visitVarStmt(const Var &) = 0;

            virtual std::any visitBlockStmt(const Block &) = 0;

            virtual std::any visitIfStmt(const If &) = 0;

            virtual std::any visitFunctionStmt(const Function &) = 0;

            virtual std::any visitReturnStmt(Return &) = 0;

            virtual std::any visitWhileStmt(const While &) = 0;
        };

        virtual ~Stmt() = default;

        virtual std::any accept(Visitor &visitor) = 0;
    };


    class Return : public Stmt {
    public:
        Token keyword;
        std::shared_ptr<Expr> value;

        Return(Token keyword, std::shared_ptr<Expr> value) : keyword(std::move(keyword)), value(std::move(value)) {
        }

        std::any accept(Visitor &visitor) override {
            return visitor.visitReturnStmt(*this);
        }
    };


    class Expression : public Stmt {
    public:
        std::shared_ptr<Expr> expression;

        explicit Expression(std::shared_ptr<Expr> expression) : expression(std::move(expression)) {
        }

        std::any accept(Stmt::Visitor &visitor) override {
            return visitor.visitSExpressionStmt(*this);
        }
    };

    class Print : public Stmt {
    public:
        std::shared_ptr<Expr> expression;

        explicit Print(std::shared_ptr<Expr> expression) : expression(std::move(expression)) {
        }

        std::any accept(Stmt::Visitor &visitor) override {
            return visitor.visitPrintStmt(*this);
        }
    };

    class Function : public Stmt {
    public:
        Token name;
        std::vector<Token> params;
        std::vector<std::shared_ptr<Stmt> > body;

        Function(Token name, Tokens params, std::vector<std::shared_ptr<Stmt> > body) : name(std::move(name)),
            params(std::move(params)), body(std::move(body)) {
        }

        std::any accept(Visitor &visitor) override {
            return visitor.visitFunctionStmt(*this);
        }
    };

    class Var : public Stmt {
    public:
        Token name;
        std::shared_ptr<Expr> initializer;

        Var(Token name, std::shared_ptr<Expr> initializer) : name(std::move(name)),
                                                             initializer(std::move(initializer)) {
        }

        std::any accept(Stmt::Visitor &visitor) override {
            return visitor.visitVarStmt(*this);
        }
    };

    class Block : public Stmt {
    public:
        std::vector<std::shared_ptr<Stmt> > statements;

        explicit Block(std::vector<std::shared_ptr<Stmt> > statements) : statements(std::move(statements)) {
        }

        std::any accept(Stmt::Visitor &visitor) override {
            return visitor.visitBlockStmt(*this);
        }
    };

    class If : public Stmt {
    public:
        std::shared_ptr<Expr> condition;
        std::shared_ptr<Stmt> thenBranch;
        std::shared_ptr<Stmt> elseBranch;

        If(std::shared_ptr<Expr> condition, std::shared_ptr<Stmt> thenBranch,
           std::shared_ptr<Stmt> elseBranch) : condition(std::move(condition)), thenBranch(std::move(thenBranch)),
                                               elseBranch(std::move(elseBranch)) {
        }

        std::any accept(Stmt::Visitor &visitor) override {
            return visitor.visitIfStmt(*this);
        }
    };

    class While : public Stmt {
    public:
        std::shared_ptr<Expr> condition;
        std::shared_ptr<Stmt> body;

        While(std::shared_ptr<Expr> condition, std::shared_ptr<Stmt> body) : condition(std::move(condition)),
                                                                             body(std::move(body)) {
        }

        std::any accept(Stmt::Visitor &visitor) override {
            return visitor.visitWhileStmt(*this);
        }
    };

    using Statement = std::shared_ptr<Stmt>;
    using SExpression = std::shared_ptr<Expr>;
    using TokenTypes = std::vector<TokenType>;
    using Stmts = std::vector<Statement>;
    using Expressions = std::vector<SExpression>;
    class LoxFunction;

    class Scanner {
        int start_{0};
        int current_{0};
        int line_{1};
        std::string_view code_;
        Tokens tokens_;

        std::map<std::string, TokenType> keywords_;

        [[nodiscard]] bool isEnd() const {
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
            const int size = current_ - start_ - 2;
            std::string lexeme{code_.substr(start_ + 1, size)};
            addToken(TokenType::STRING, lexeme);
        }

        [[nodiscard]] static bool isDigit(const char c) {
            return '0' <= c && c <= '9';
        }

        [[nodiscard]] char peek() const {
            if (isEnd()) return '\0';
            return code_[current_];
        }

        [[nodiscard]] char peekNext() const {
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
            const std::string lexeme{code_.substr(start_, digSize)};
            addToken(TokenType::NUMBER, std::stod(lexeme));
        }

        void addToken(const TokenType type) {
            addToken(type, nullptr);
        }

        void addToken(TokenType type, const std::any &literal) {
            const int tokenSize = current_ - start_;
            std::string text{code_.substr(start_, tokenSize)};
            tokens_.emplace_back(type, text, literal, line_);
        }


        void identifier() {
            while (isAlphaNum(peek())) advance();
            const int identifierSize = current_ - start_;
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


    class Parser {
        Tokens tokens_;
        int current{0};

        SExpression term() {
            SExpression expr = factor();

            while (match({TokenType::MINUS, TokenType::PLUS})) {
                Token op = previous();
                SExpression right = factor();
                expr = std::make_shared<Binary>(expr, op, right);
            }

            return expr;
        }

        SExpression factor() {
            SExpression expr = unary();

            while (match({TokenType::SLASH, TokenType::STAR})) {
                Token op = previous();
                SExpression right = unary();
                expr = std::make_shared<Binary>(expr, op, right);
            }

            return expr;
        }

        SExpression finishCall(const SExpression &callee) {
            Expressions args;
            if (!check(TokenType::RIGHT_PAREN)) {
                do {
                    args.push_back(expression());
                } while (match({TokenType::COMMA}));
            }

            Token par = consume(TokenType::RIGHT_PAREN, "Expect ) after callable arguments.");
            return std::make_shared<Call>(callee, par, args);
        }

        SExpression call() {
            SExpression expr = primary();

            while (true) {
                if (match({TokenType::LEFT_PAREN})) {
                    expr = finishCall(expr);
                } else {
                    break;
                }
            }

            return expr;
        }

        SExpression unary() {
            if (match({TokenType::BANG, TokenType::MINUS})) {
                Token op = previous();
                SExpression right = unary();
                return std::make_shared<Unary>(op, right);
            }

            return call();
        }


        SExpression equality() {
            SExpression expr = comparison();

            while (match({TokenType::BANG_EQUAL, TokenType::EQUAL_EQUAL})) {
                Token op = previous();
                SExpression right = comparison();
                expr = std::make_shared<Binary>(expr, op, right);
            }

            return expr;
        }

        SExpression primary() {
            if (match({TokenType::FALSE})) return std::make_shared<Literal>(false);
            if (match({TokenType::TRUE})) return std::make_shared<Literal>(true);
            if (match({TokenType::NIL})) return std::make_shared<Literal>(nullptr);

            if (match({TokenType::NUMBER, TokenType::STRING}))
                return std::make_shared<
                    Literal>(previous().getLiteral());

            if (match({TokenType::IDENTIFIER})) {
                return std::make_shared<Variable>(previous());
            }

            if (match({TokenType::LEFT_PAREN})) {
                SExpression expr = expression();
                consume(TokenType::RIGHT_PAREN, "Expect ')' after expression");
                return std::make_shared<Grouping>(expr);
            }

            throw std::logic_error("Expected expression"); // TODO: Better error handling :cry
        }

        Token consume(const TokenType &type, const std::string &message) {
            if (check(type)) return advance();
            error(previous(), message);
            throw std::logic_error(message);
        }

        static void error(const Token &token, const std::string &message) {
            if (token.getType() == TokenType::EOFF) {
                report(token.getLine(), " at end ", message);
            } else {
                report(token.getLine(), " at '" + token.getLexeme() + "'", message);
            }
        }

        void synchronize() {
            advance();

            while (!isEnd()) {
                if (previous().getType() == TokenType::SEMICOLON) return;

                switch (peek().getType()) {
                    case TokenType::CLASS:
                    case TokenType::FUN:
                    case TokenType::VAR:
                    case TokenType::FOR:
                    case TokenType::IF:
                    case TokenType::WHILE:
                    case TokenType::PRINT:
                    case TokenType::RETURN:
                        return;
                    default:
                        break;
                }

                advance();
            }
        }

        SExpression comparison() {
            SExpression expr = term();

            while (match({
                TokenType::GREATER, TokenType::GREATER_EQUAL, TokenType::LESS, TokenType::LESS_EQUAL, TokenType::AND,
                TokenType::OR
            })) {
                Token op = previous();
                SExpression right = term();
                expr = std::make_shared<Binary>(expr, op, right);
            }

            return expr;
        }

        SExpression gAnd() {
            SExpression expr = equality();

            while (match({TokenType::AND})) {
                Token op = previous();
                SExpression right = equality();
                expr = std::make_shared<Logical>(expr, op, right);
            }

            return expr;
        }

        SExpression gOr() {
            SExpression expr = gAnd();

            while (match({TokenType::OR})) {
                Token op = previous();
                SExpression right = equality();
                expr = std::make_shared<Logical>(expr, op, right);
            }

            return expr;
        }

        SExpression assignment() {
            SExpression expression = gOr();

            if (match({TokenType::EQUAL})) {
                Token equals = previous();
                SExpression value = assignment();

                if (auto v = dynamic_cast<Variable *>(expression.get())) {
                    return std::make_shared<Assign>(v->name, value);
                }
            }

            return expression;
        }

        SExpression expression() {
            return assignment();
            // SExpression expr = comparison();
            //
            // while (match({TokenType::BANG_EQUAL, TokenType::EQUAL_EQUAL})) {
            //     Token op = previous();
            //     SExpression right = comparison();
            //     expr = std::make_shared<Binary>(expr, op, right);
            // }
            //
            // return expr;
        }

        [[nodiscard]] Token peek() const {
            return tokens_[current];
        }

        [[nodiscard]] bool isEnd() const {
            return peek().getType() == TokenType::EOFF;
        }

        Token previous() {
            return tokens_[current - 1];
        }

        Token advance() {
            if (!isEnd()) current++;
            return previous();
        }

        [[nodiscard]] bool check(const TokenType type) const {
            if (isEnd()) return false;
            return peek().getType() == type;
        }

        bool match(const TokenTypes &tokens) {
            for (const auto &token: tokens) {
                if (check(token)) {
                    advance();
                    return true;
                }
            }

            return false;
        }

        Statement printStmt() {
            SExpression value = expression();
            consume(TokenType::SEMICOLON, "Expect ; after value");
            return std::make_shared<Print>(value);
        }

        Statement expressionStmt() {
            SExpression value = expression();
            consume(TokenType::SEMICOLON, "Expect ; after expression");
            return std::make_shared<Expression>(value);
        }

        Stmts block() {
            Stmts statements;

            while (!check(TokenType::RIGHT_BRACE) && !isEnd()) {
                statements.push_back(declaration());
            }

            consume(TokenType::RIGHT_BRACE, "EXPECT } after block.");
            return statements;
        }

        Statement ifStatement() {
            consume(TokenType::LEFT_PAREN, "Expect ( after the 'if' statement.");
            SExpression condition = expression();
            consume(TokenType::RIGHT_PAREN, "Expect ) after the condition in an if statement");

            Statement thenBranch = statement();
            Statement elseBranch = nullptr;
            if (match({TokenType::ELSE})) {
                elseBranch = statement();
            }

            return std::make_shared<If>(condition, thenBranch, elseBranch);
        }

        Statement whileStatement() {
            consume(TokenType::LEFT_PAREN, "Expect ( after while statement");
            SExpression condition = expression();
            consume(TokenType::RIGHT_PAREN, "Expect ) after the end of condition");
            Statement body = statement();
            return std::make_shared<While>(condition, body);
        }


        // this funciton is changing for (int i = 0; i < n; i++) to
        // var i = 0; while (i < n) { i += 1;}
        Statement forStatement() {
            consume(TokenType::LEFT_PAREN, "Expect ( after for statement");

            // for -> initializer | none; condition | none; statement | null
            Statement initializer;
            if (match({TokenType::SEMICOLON})) {
                initializer = nullptr;
            } else if (match({TokenType::VAR})) {
                initializer = varDeclaration();
            } else {
                initializer = expressionStmt();
            }

            SExpression condition = nullptr;
            if (!check(TokenType::SEMICOLON)) {
                condition = expression();
            }

            consume(TokenType::SEMICOLON, "Expect ; after a loop condition");

            SExpression increment = nullptr;
            if (!check(TokenType::RIGHT_PAREN)) {
                increment = expression();
            }

            consume(TokenType::RIGHT_PAREN, "Expect ) after the end of a loop statement");

            Statement body = statement();

            if (increment != nullptr) {
                body = std::make_shared<Block>(Stmts{body, std::make_shared<Expression>(increment)});
            }

            if (condition == nullptr) condition = std::make_shared<Literal>(true);
            body = std::make_shared<While>(condition, body);

            if (initializer != nullptr) {
                body = std::make_shared<Block>(Stmts{initializer, body});
            }

            return body;
        }

        Statement returnStatement() {
            Token keyword = previous();
            SExpression value = nullptr;

            if (!check(TokenType::SEMICOLON)) {
                value = expression();
            }

            consume(TokenType::SEMICOLON, "Expect ; after the return statement");
            return std::make_shared<Return>(keyword, value);
        }

        Statement statement() {
            if (match({TokenType::IF})) {
                return ifStatement();
            }
            if (match({TokenType::PRINT})) {
                return printStmt();
            }

            if (match({TokenType::RETURN})) {
                return returnStatement();
            }

            if (match({TokenType::WHILE})) {
                return whileStatement();
            }
            if (match({TokenType::FOR})) {
                return forStatement();
            }
            if (match({TokenType::LEFT_BRACE})) {
                return std::make_shared<Block>(block());
            }
            return expressionStmt();
        }

        Statement varDeclaration() {
            Token name = consume(TokenType::IDENTIFIER, "Expect variable name.");

            SExpression initializer = nullptr;
            if (match({TokenType::EQUAL})) {
                initializer = expression();
            }

            consume(TokenType::SEMICOLON, "Expect ; after delcaration.");
            return std::make_shared<Var>(name, initializer);
        }

        Statement funDefinition(const std::string &kind = "function") {
            Token name = consume(TokenType::IDENTIFIER, "Expect  " + kind + " name.");

            consume(TokenType::LEFT_PAREN, "Expect ( after " + kind + "name.");
            Tokens params;
            if (!check(TokenType::RIGHT_PAREN)) {
                do {
                    // limit parameters size but why?
                    params.push_back(consume(TokenType::IDENTIFIER, "Expect parameter name"));
                } while (match({TokenType::COMMA}));
            }
            consume(TokenType::RIGHT_PAREN, "Expect ) after " + kind + " parameters");

            consume(TokenType::LEFT_BRACE, "Expect { before " + kind + " body.");
            Stmts body = block();
            return std::make_shared<Function>(name, params, body);
        }

        Statement declaration() {
            try {
                if (match({TokenType::FUN})) return funDefinition();
                if (match({TokenType::VAR})) return varDeclaration();

                return statement();
            } catch (...) {
                synchronize();
                return nullptr;
            }
        }

    public:
        explicit Parser(Tokens tokens) : tokens_(std::move(tokens)) {
        }

        Stmts parse() {
            Stmts stmts;

            while (!isEnd()) {
                stmts.push_back(declaration());
            }

            return stmts;
        }
    };

    class AstPrinter : public Visitor {
    public:
        std::string print(const SExpression &expr) {
            return std::any_cast<std::string>(expr->accept(*this));
        }

        std::any visitBinaryExpr(Binary &e) override {
            return parenthesize(e.op.getLexeme(), {e.left, e.right});
        }

        std::any visitGroupingExpr(Grouping &e) override {
            return parenthesize("group", {e.expression});
        }

        std::any visitLiteralExpr(Literal &e) override {
            if (!e.value.has_value()) return std::string("nil");

            if (e.value.type() == typeid(std::string)) {
                return std::any_cast<std::string>(e.value);
            }

            if (e.value.type() == typeid(double)) {
                return std::to_string(std::any_cast<double>(e.value));
            }

            if (e.value.type() == typeid(bool)) {
                return std::any_cast<bool>(e.value) ? std::string("true") : std::string("false");
            }

            return std::string("?");
        }

        std::any visitUnaryExpr(Unary &e) override {
            return parenthesize(e.op.getLexeme(), {e.right});
        }

    private:
        std::string parenthesize(const std::string &name, const std::vector<SExpression> &expressions) {
            std::string result = "(" + name;
            for (const auto &expr: expressions) {
                result += " ";
                result += std::any_cast<std::string>(expr->accept(*this));
            }
            result += ")";
            return result;
        }
    };

    // TODO: please use isAddSupported, isStarSupported, isCompSupported, isSlashSupported and others to help for validation.
    class Helper {
    public:
        static bool isAddSupported(const std::any &left, const std::any &right) {
            if (!isSameType(left, right)) return false; // just for now
            return isNumber(left) || isString(left);
        }

        static bool isStarSupported(const std::any &left, const std::any &right) {
            return isSameType(left, right) && isNumber(left);
        }

        // Use it when you have is greater, is less, is equal, is
        static bool isCompSupported(const std::any &left, const std::any &right) {
            if (!isSameType(left, right)) return false;
            return isNumber(left) || isBoolean(left) || isString(left);
        }

        static bool isSlashSupported(const std::any &left, const std::any &right) {
            if (!isSameType(left, right)) return false;
            return isNumber(left);
        }

        static bool isNumber(const std::any &x) noexcept {
            return x.type() == typeid(int) || x.type() == typeid(double);
        }

        static bool isString(const std::any &x) noexcept {
            return x.type() == typeid(std::string);
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
                return std::string{""};
            }
        }

        static bool isNull(const std::any &x) {
            return x.type() == typeid(std::nullptr_t);
        }

        static std::string toString(const std::any &x) {
            if (isNumber(x)) return std::to_string(getNumber(x));
            if (isBoolean(x)) return (getBoolean(x) == true ? "true" : "false");
            if (isString(x)) return getString(x);
            if (isNull(x)) return "nullptr";
            return "unknown or unsupported value"; // or call internal implementation of to string
        }


        static bool isTruthy(const std::any &obj) noexcept {
            if (!obj.has_value()) return false;

            if (obj.type() == typeid(bool))
                return std::any_cast<bool>(obj);

            if (obj.type() == typeid(double))
                return std::any_cast<double>(obj) != static_cast<double>(0);

            if (obj.type() == typeid(std::string))
                return !std::any_cast<std::string>(obj).empty();

            return true;
        }

        static bool isBoolean(const std::any &x) {
            return x.type() == typeid(bool);
        }

        static bool getBoolean(const std::any &x) {
            assert(isBoolean(x));
            try {
                return std::any_cast<bool>(x);
            } catch (...) {
                return false;
            }
        }

        static bool isEqual(const std::any &left, const std::any &right) {
            if (!isSameType(left, right)) return false;
            if (isNumber(left)) return getNumber(left) == getNumber(right);
            if (isString(left)) return getString(left) == getString(right);
            if (isBoolean(left)) return getBoolean(left) == getBoolean(right);
            return false; // TODO: panic broddy
        }

        static bool isGreater(const std::any &left, const std::any &right) {
            if (!isSameType(left, right)) return false;
            if (isNumber(left)) return getNumber(left) > getNumber(right);
            if (isString(left)) return getString(left) > getString(right);
            return false; // TODO: panic here please or introduce type deduction
        }

        static bool isLess(const std::any &left, const std::any &right) {
            if (!isSameType(left, right) || isEqual(left, right)) return false;
            if (isGreater(left, right)) return false;
            return true;
        }

        static bool isOr(const std::any &left, const std::any &right) {
            return isTruthy(left) || isTruthy(right);
        }

        static bool isAnd(const std::any &left, const std::any &right) {
            return isTruthy(left) && isTruthy(right);
        }

        static bool isGreaterEqual(const std::any &left, const std::any &right) {
            return isGreater(left, right) || isEqual(left, right);
        }

        static bool isLessEqual(const std::any &left, const std::any &right) {
            return isLess(left, right) || isEqual(left, right);
        }

        static bool isSameType(const std::any &left, const std::any &right) noexcept {
            return left.type() == right.type();
        }
    };

    class Environment {
    public:
        std::unique_ptr<Environment> enclosing;
        std::map<std::string, std::any> values;

        explicit Environment(std::unique_ptr<Environment> enclosing = nullptr) : enclosing(std::move(enclosing)),
            values(std::map<std::string, std::any>{}) {
        }

        void define(const std::string &name, std::any value) {
            // if (values.contains(name)) {
            //     throw RuntimeError{};
            // }

            values[name] = std::move(value);
        }

        [[nodiscard]] std::any get(const std::string &name) const {
            if (values.contains(name)) {
                return values.at(name);
            }

            if (enclosing != nullptr) {
                return enclosing->get(name);
            }

            throw RuntimeError();
        }

        void assign(const std::string &name, const std::any &value) {
            if (values.contains(name)) {
                values[name] = value;
                return;
            }

            if (enclosing != nullptr) {
                enclosing->assign(name, value);
                return;
            }

            throw RuntimeError();
        }
    };

    class Interpreter;

    class LoxCallable {
    public:
        virtual ~LoxCallable() = default;

        [[nodiscard]] virtual size_t arity() const = 0;

        virtual std::any call(Interpreter &interpreter, const std::vector<std::any> &arguments) const = 0;
    };


    class NativeFunction : public LoxCallable {
    public:
        using Fn = std::function<std::any(Interpreter &, const std::vector<std::any> &)>;

        NativeFunction(const size_t arity, Fn fn)
            : arity_(arity), fn_(std::move(fn)) {
        }

        [[nodiscard]] size_t arity() const override { return arity_; }

        std::any call(Interpreter &interpreter,
                      const std::vector<std::any> &args) const override {
            return fn_(interpreter, args);
        }

    private:
        size_t arity_;
        Fn fn_;
    };

    class LoxFunction : public LoxCallable {
    public:
        explicit LoxFunction(std::shared_ptr<Function> func) : declaration(std::move(func)) {
        }

        std::any call(Interpreter &interpreter, const std::vector<std::any> &arguments) const override;

        [[nodiscard]] size_t arity() const override {
            return declaration->params.size();
        }

    private:
        std::shared_ptr<Function> declaration;
    };

    using Func = std::shared_ptr<Function>;

    class ReturnException : public std::exception {
    public:
        std::any value;

        explicit ReturnException(std::any value) : value(std::move(value)) {
        }
    };

    class Interpreter : public Visitor, public Stmt::Visitor {
        Environment env;
        Environment globals;


        std::any visitReturnStmt(Return &stmt) override {
            std::any value = nullptr;
            if (!Helper::isNull(stmt.value)) value = evaluate(stmt.value);
            throw ReturnException{value};
        }

        std::any visitCallExpr(Call &expr) override {
            const std::any callee = evaluate(expr.callee);

            using Args = std::vector<std::any>;
            Args args;

            for (const auto &arg: expr.arguments) {
                args.push_back(evaluate(arg));
            }

            try {
                // Wrap the LoxFunction in a shared_ptr
                auto function = std::any_cast<std::shared_ptr<LoxCallable> >(callee);

                if (args.size() != function->arity()) {
                    throw RuntimeError{};
                }

                return function->call(*this, args);
            } catch (const std::bad_any_cast &e) {
                throw RuntimeError{};
            } catch (const std::bad_cast &e) {
                throw RuntimeError{};
            }
        }

        std::any visitIfStmt(const If &stmt) override {
            if (Helper::isTruthy(evaluate(stmt.condition))) {
                execute(stmt.thenBranch);
            } else if (stmt.elseBranch != nullptr) {
                execute(stmt.elseBranch);
            }

            return nullptr;
        }

        std::any visitLogicalExpr(Logical &expr) override {
            std::any left = evaluate(expr.left);

            if (expr.op.getType() == TokenType::AND) {
                if (!Helper::isTruthy(left)) return left;
            } else {
                if (Helper::isTruthy(left)) return left;
            }

            return evaluate(expr.right);
        }

        std::any visitBlockStmt(const Block &block) override {
            executeBlock(block.statements);
            // executeBlock(block.statements, Environment{std::make_unique<Environment>(env)});
            return nullptr;
        }

        std::any visitLiteralExpr(Literal &exp) override {
            return exp.value;
        }

        std::any visitWhileStmt(const While &stmt) override {
            while (Helper::isTruthy(evaluate(stmt.condition))) {
                execute(stmt.body);
            }

            return nullptr;
        }

        std::any visitSExpressionStmt(const Expression &stmt) override {
            evaluate(stmt.expression);
            return nullptr;
        }

        std::any visitVariableExpr(Variable &expr) override {
            return env.get(expr.name.getLexeme());
        }

        std::any visitAssignExpr(Assign &expr) override {
            std::any value = evaluate(expr.value);
            env.assign(expr.name.getLexeme(), value);
            return value;
        }

        std::any visitVarStmt(const Var &stmt) override {
            std::any value = nullptr;
            if (stmt.initializer != nullptr) {
                value = evaluate(stmt.initializer);
            }

            env.define(stmt.name.getLexeme(), value);
            return nullptr;
        }

        std::any visitPrintStmt(const Print &stmt) override {
            const std::any value = evaluate(stmt.expression);
            std::cout << Helper::toString(value) << std::endl;
            return nullptr;
        }

        std::any visitGroupingExpr(Grouping &exp) override {
            return evaluate(exp.expression);
        }

        std::any evaluate(const SExpression &expr) {
            return expr->accept(*this);
        }

        std::any visitUnaryExpr(Unary &expr) override {
            const std::any right = evaluate(expr.right);

            switch (expr.op.getType()) {
                case TokenType::MINUS: {
                    return -std::any_cast<double>(right);
                }
                case TokenType::BANG: {
                    return !Helper::isTruthy(right);
                }
                default: return nullptr;
            }

            assert(false && "reached here");
        }

        void execute(const Statement &stmt) {
            stmt->accept(*this);
        }

        std::any visitBinaryExpr(Binary &expr) override {
            const std::any left = evaluate(expr.left);
            const std::any right = evaluate(expr.right);

            auto get_double = [](const std::any &value) {
                return std::any_cast<double>(value);
            };

            switch (expr.op.getType()) {
                case TokenType::MINUS: {
                    if (!Helper::isSameType(left, right)) {
                        // TODO: panic
                    }
                    return get_double(left) - get_double(right);
                }
                case TokenType::PLUS: {
                    if (!Helper::isSameType(left, right)) {
                        // TODO: panic
                    }
                    return get_double(left) + get_double(right);
                }
                case TokenType::STAR: {
                    if (!Helper::isSameType(left, right)) {
                        // TODO: panic
                    }
                    return get_double(left) * get_double(right);
                }
                case TokenType::SLASH: {
                    if (!Helper::isSameType(left, right)) {
                    }

                    return get_double(left) / get_double(right);
                }
                case TokenType::BANG_EQUAL: {
                    return !Helper::isEqual(left, right);
                }
                case TokenType::EQUAL_EQUAL: {
                    return Helper::isEqual(left, right);
                }
                case TokenType::GREATER: {
                    return Helper::isGreater(left, right);
                }
                case TokenType::LESS: {
                    return Helper::isLess(left, right);
                }
                case TokenType::GREATER_EQUAL: {
                    return Helper::isGreaterEqual(left, right);
                }

                case TokenType::LESS_EQUAL: {
                    return Helper::isLessEqual(left, right);
                }
                default: ;
            }

            return nullptr;
        }

        std::any visitFunctionStmt(const Function &stmt) override {
            auto decl = std::make_shared<Function>(stmt);
            auto function = std::make_shared<LoxFunction>(decl);

            std::shared_ptr<LoxCallable> callable = function;
            env.define(stmt.name.getLexeme(), std::make_any<std::shared_ptr<LoxCallable> >(callable));

            return nullptr;
        }

    public:
        void executeBlock(const Stmts &statements) {
            try {
                for (const auto &statement: statements) {
                    execute(statement);
                }
            } catch (ReturnException &val) {
                throw val;
            } catch (...) {
                return;
            }
        }

        void interpret(const SExpression &expr) {
            try {
                const auto value = evaluate(expr);
                std::cout << "Value is " << Helper::toString(value) << std::endl;
            } catch (std::exception &e) {
                std::cout << "Some error happened" << e.what() << std::endl;
            }
        }

        void interpret(const Stmts &stmts) {
            try {
                for (const auto &stmt: stmts) {
                    execute(stmt);
                }
            } catch (...) {
                std::cerr << "ERROR: happened" << std::endl;
            }
        }

        Interpreter() {
            env = Environment{};
            globals.define(
                "clock",
                std::make_shared<NativeFunction>(
                    0,
                    [](Interpreter &, const std::vector<std::any> &) {
                        using namespace std::chrono;
                        return duration<double>(
                            system_clock::now().time_since_epoch()
                        ).count();
                    }
                )
            );
        }
    };


    void run(const std::string_view &code) {
        Scanner scanner{code};
        const Tokens tokens{scanner.scan()};
        if (hasError_) {
            exit(2);
        }


        Parser parser{tokens};
        const auto stmts = parser.parse();

        // const SExpression expr = parser.parse();
        // AstPrinter printer;
        // std::cout << printer.print(expr) << std::endl;

        Interpreter interpreter;

        interpreter.interpret(stmts);
    }

    std::any LoxFunction::call(Interpreter &interpreter, const std::vector<std::any> &arguments) const {
        // Now the compiler knows that interpreter has an executeBlock method
        Environment env;
        for (size_t i = 0; i < declaration->params.size(); i++) {
            env.define(declaration->params[i].getLexeme(), arguments.at(i));
        }

        try {
            interpreter.executeBlock(declaration->body);
        } catch (ReturnException &e) {
            return e.value;
        }

        return nullptr;
    }

    void runFile(const std::string &fileName) {
        std::ifstream infile{fileName};

        if (!infile.is_open()) {
            throw std::runtime_error("could not open file " + fileName);
        }

        // TODO: rather than loading all the fileContent make sure to parse the source code line by line
        const std::string sCode{std::istreambuf_iterator<char>(infile), std::istreambuf_iterator<char>()};
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
