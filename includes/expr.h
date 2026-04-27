#pragma once

#include "token.h"
#include <any>
#include <memory>
#include <vector>

namespace cpplox {

class Binary;
class Grouping;
class Literal;
class Unary;
class Variable;
class Assign;
class Logical;
class Call;
class Get;
class Set;
class This;
class Super;

class Visitor {
public:
    virtual ~Visitor() = default;

    virtual std::any visitBinaryExpr(Binary &) = 0;
    virtual std::any visitGroupingExpr(Grouping &) = 0;
    virtual std::any visitLiteralExpr(Literal &) = 0;
    virtual std::any visitUnaryExpr(Unary &) = 0;
    virtual std::any visitVariableExpr(Variable &) = 0;
    virtual std::any visitAssignExpr(Assign &) = 0;
    virtual std::any visitLogicalExpr(Logical &) = 0;
    virtual std::any visitCallExpr(Call &) = 0;
    virtual std::any visitGetExpr(Get &) = 0;
    virtual std::any visitSetExpr(Set &) = 0;
    virtual std::any visitThisExpr(This &) = 0;
    virtual std::any visitSuperExpr(Super &) = 0;
};

class Expr {
public:
    virtual ~Expr() = default;
    virtual std::any accept(Visitor &) = 0;
};

using SExpression = std::shared_ptr<Expr>;
using Expressions = std::vector<SExpression>;

class Binary : public Expr {
public:
    Binary(std::shared_ptr<Expr> left, Token op, std::shared_ptr<Expr> right)
        : left_(std::move(left)), op_(std::move(op)), right_(std::move(right)) {}

    std::any accept(Visitor &visitor) override {
        return visitor.visitBinaryExpr(*this);
    }

    const std::shared_ptr<Expr> left_;
    const Token op_;
    const std::shared_ptr<Expr> right_;
};

class Call : public Expr {
public:
    Call(std::shared_ptr<Expr> callee, Token par, std::vector<std::shared_ptr<Expr>> arguments)
        : callee_(std::move(callee)), par_(std::move(par)), arguments_(std::move(arguments)) {}

    std::any accept(Visitor &visitor) override {
        return visitor.visitCallExpr(*this);
    }

    std::shared_ptr<Expr> callee_;
    Token par_;
    std::vector<std::shared_ptr<Expr>> arguments_;
};

class Get : public Expr {
public:
    Get(std::shared_ptr<Expr> object, Token name)
        : object_(std::move(object)), name_(std::move(name)) {}

    std::any accept(Visitor &visitor) override {
        return visitor.visitGetExpr(*this);
    }

    std::shared_ptr<Expr> object_;
    Token name_;
};

class Set : public Expr {
public:
    Set(std::shared_ptr<Expr> object, Token name, std::shared_ptr<Expr> value)
        : object_(std::move(object)), name_(std::move(name)), value_(std::move(value)) {}

    std::any accept(Visitor &visitor) override {
        return visitor.visitSetExpr(*this);
    }

    std::shared_ptr<Expr> object_;
    Token name_;
    std::shared_ptr<Expr> value_;
};

class This : public Expr {
public:
    explicit This(Token keyword) : keyword_(std::move(keyword)) {}

    std::any accept(Visitor &visitor) override {
        return visitor.visitThisExpr(*this);
    }

    Token keyword_;
};

class Super : public Expr {
public:
    Super(Token keyword, Token method) : keyword_(std::move(keyword)), method_(std::move(method)) {}

    std::any accept(Visitor &visitor) override {
        return visitor.visitSuperExpr(*this);
    }

    Token keyword_;
    Token method_;
};

class Grouping : public Expr {
public:
    explicit Grouping(std::shared_ptr<Expr> expression) : expression_(std::move(expression)) {}

    std::any accept(Visitor &visitor) override {
        return visitor.visitGroupingExpr(*this);
    }

    const std::shared_ptr<Expr> expression_;
};

class Literal : public Expr {
public:
    explicit Literal(std::any value) : value_(std::move(value)) {}

    std::any accept(Visitor &visitor) override {
        return visitor.visitLiteralExpr(*this);
    }

    const std::any value_;
};

class Logical : public Expr {
public:
    Logical(std::shared_ptr<Expr> left, Token op, std::shared_ptr<Expr> right)
        : left_(std::move(left)), op_(std::move(op)), right_(std::move(right)) {}

    std::any accept(Visitor &visitor) override {
        return visitor.visitLogicalExpr(*this);
    }

    std::shared_ptr<Expr> left_;
    Token op_;
    std::shared_ptr<Expr> right_;
};

class Unary : public Expr {
public:
    Unary(Token op, std::shared_ptr<Expr> right) : op_(std::move(op)), right_(std::move(right)) {}

    std::any accept(Visitor &visitor) override {
        return visitor.visitUnaryExpr(*this);
    }

    const Token op_;
    const std::shared_ptr<Expr> right_;
};

class Variable : public Expr {
public:
    explicit Variable(Token name) : name_(std::move(name)) {}

    std::any accept(Visitor &visitor) override {
        return visitor.visitVariableExpr(*this);
    }

    Token name_;
};

class Assign : public Expr {
public:
    Assign(Token name, std::shared_ptr<Expr> value) : name_(std::move(name)), value_(std::move(value)) {}

    std::any accept(Visitor &visitor) override {
        return visitor.visitAssignExpr(*this);
    }

    Token name_;
    std::shared_ptr<Expr> value_;
};

} // namespace cpplox
