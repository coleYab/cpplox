#pragma once

#include "error.h"
#include "expr.h"
#include "stmt.h"
#include "token.h"
#include <algorithm>
#include <stdexcept>

namespace cpplox {

class Parser {
    Tokens tokens_;
    size_t current{0};

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
            } else if (match({TokenType::DOT})) {
                Token name = consume(TokenType::IDENTIFIER, "Expect property name after '.'.");
                expr = std::make_shared<Get>(expr, name);
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
            return std::make_shared<Literal>(previous().getLiteral());

        if (match({TokenType::IDENTIFIER})) {
            return std::make_shared<Variable>(previous());
        }

        if (match({TokenType::THIS})) {
            return std::make_shared<This>(previous());
        }

        if (match({TokenType::SUPER})) {
            Token keyword = previous();
            consume(TokenType::DOT, "Expect '.' after 'super'.");
            Token method = consume(TokenType::IDENTIFIER, "Expect superclass method name.");
            return std::make_shared<Super>(keyword, method);
        }

        if (match({TokenType::LEFT_PAREN})) {
            SExpression expr = expression();
            consume(TokenType::RIGHT_PAREN, "Expect ')' after expression");
            return std::make_shared<Grouping>(expr);
        }

        throw std::logic_error("Expected expression");
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
                return std::make_shared<Assign>(v->name_, value);
            }
            if (auto g = dynamic_cast<Get *>(expression.get())) {
                return std::make_shared<Set>(g->object_, g->name_, value);
            }
        }

        return expression;
    }

    SExpression expression() {
        return assignment();
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
        const auto ok = std::ranges::any_of(tokens, [this](const auto &token) {
            return check(token);
        });

        if (ok) advance();

        return ok;
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

    Statement forStatement() {
        consume(TokenType::LEFT_PAREN, "Expect ( after for statement");

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

        consume(TokenType::SEMICOLON, "Expect ; after declaration.");
        return std::make_shared<Var>(name, initializer);
    }

    Statement funDefinition(const std::string &kind = "function") {
        Token name = consume(TokenType::IDENTIFIER, "Expect  " + kind + " name.");

        consume(TokenType::LEFT_PAREN, "Expect ( after " + kind + "name.");
        Tokens params;
        if (!check(TokenType::RIGHT_PAREN)) {
            do {
                params.push_back(consume(TokenType::IDENTIFIER, "Expect parameter name"));
            } while (match({TokenType::COMMA}));
        }
        consume(TokenType::RIGHT_PAREN, "Expect ) after " + kind + " parameters");

        consume(TokenType::LEFT_BRACE, "Expect { before " + kind + " body.");
        Stmts body = block();
        return std::make_shared<Function>(name, params, body);
    }

    Statement classDeclaration() {
        Token name = consume(TokenType::IDENTIFIER, "Expect class name.");

        std::shared_ptr<Variable> superclass = nullptr;
        if (match({TokenType::LESS})) {
            consume(TokenType::IDENTIFIER, "Expect superclass name.");
            superclass = std::make_shared<Variable>(previous());
        }

        consume(TokenType::LEFT_BRACE, "Expect '{' before class body.");

        std::vector<std::shared_ptr<Function>> methods;
        while (!check(TokenType::RIGHT_BRACE) && !isEnd()) {
            methods.push_back(std::dynamic_pointer_cast<Function>(funDefinition("method")));
        }

        consume(TokenType::RIGHT_BRACE, "Expect '}' after class body.");

        return std::make_shared<ClassStmt>(name, superclass, methods);
    }

    Statement declaration() {
        try {
            if (match({TokenType::CLASS})) return classDeclaration();
            if (match({TokenType::FUN})) return funDefinition();
            if (match({TokenType::VAR})) return varDeclaration();

            return statement();
        } catch (...) {
            synchronize();
            return nullptr;
        }
    }

public:
    explicit Parser(Tokens tokens) : tokens_(std::move(tokens)) {}

    Stmts parse() {
        Stmts stmts;

        while (!isEnd()) {
            stmts.push_back(declaration());
        }

        return stmts;
    }
};

} // namespace cpplox
