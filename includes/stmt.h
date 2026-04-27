#pragma once

#include "expr.h"
#include <memory>
#include <vector>

namespace cpplox {

class Expression;
class Print;
class Var;
class Block;
class If;
class While;
class Function;
class Return;
class ClassStmt;

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
        virtual std::any visitReturnStmt(const Return &) = 0;
        virtual std::any visitWhileStmt(const While &) = 0;
        virtual std::any visitClassStmt(const ClassStmt &) = 0;
    };

    virtual ~Stmt() = default;
    virtual std::any accept(Visitor &) = 0;
};

using Statement = std::shared_ptr<Stmt>;
using Stmts = std::vector<Statement>;
using Func = std::shared_ptr<Function>;

class Return : public Stmt {
public:
    Return(Token keyword, std::shared_ptr<Expr> value)
        : keyword_(std::move(keyword)), value_(std::move(value)) {}

    std::any accept(Visitor &visitor) override {
        return visitor.visitReturnStmt(*this);
    }

    Token keyword_;
    std::shared_ptr<Expr> value_;
};

class Expression : public Stmt {
public:
    explicit Expression(std::shared_ptr<Expr> expression) : expression_(std::move(expression)) {}

    std::any accept(Visitor &visitor) override {
        return visitor.visitSExpressionStmt(*this);
    }

    std::shared_ptr<Expr> expression_;
};

class Print : public Stmt {
public:
    explicit Print(std::shared_ptr<Expr> expression) : expression_(std::move(expression)) {}

    std::any accept(Visitor &visitor) override {
        return visitor.visitPrintStmt(*this);
    }

    std::shared_ptr<Expr> expression_;
};

class Function : public Stmt {
public:
    Function(Token name, Tokens params, std::vector<std::shared_ptr<Stmt>> body)
        : name_(std::move(name)), params_(std::move(params)), body_(std::move(body)) {}

    std::any accept(Visitor &visitor) override {
        return visitor.visitFunctionStmt(*this);
    }

    Token name_;
    std::vector<Token> params_;
    std::vector<std::shared_ptr<Stmt>> body_;
};

class ClassStmt : public Stmt {
public:
    ClassStmt(Token name, std::shared_ptr<Variable> superclass,
              std::vector<std::shared_ptr<Function>> methods)
        : name_(std::move(name)), superclass_(std::move(superclass)), methods_(std::move(methods)) {}

    std::any accept(Visitor &visitor) override {
        return visitor.visitClassStmt(*this);
    }

    Token name_;
    std::shared_ptr<Variable> superclass_;
    std::vector<std::shared_ptr<Function>> methods_;
};

class Var : public Stmt {
public:
    Var(Token name, std::shared_ptr<Expr> initializer)
        : name_(std::move(name)), initializer_(std::move(initializer)) {}

    std::any accept(Visitor &visitor) override {
        return visitor.visitVarStmt(*this);
    }

    Token name_;
    std::shared_ptr<Expr> initializer_;
};

class Block : public Stmt {
public:
    explicit Block(std::vector<std::shared_ptr<Stmt>> statements) : statements_(std::move(statements)) {}

    std::any accept(Visitor &visitor) override {
        return visitor.visitBlockStmt(*this);
    }

    std::vector<std::shared_ptr<Stmt>> statements_;
};

class If : public Stmt {
public:
    If(std::shared_ptr<Expr> condition, std::shared_ptr<Stmt> thenBranch, std::shared_ptr<Stmt> elseBranch)
        : condition_(std::move(condition)), thenBranch_(std::move(thenBranch)), elseBranch_(std::move(elseBranch)) {}

    std::any accept(Visitor &visitor) override {
        return visitor.visitIfStmt(*this);
    }

    std::shared_ptr<Expr> condition_;
    std::shared_ptr<Stmt> thenBranch_;
    std::shared_ptr<Stmt> elseBranch_;
};

class While : public Stmt {
public:
    While(std::shared_ptr<Expr> condition, std::shared_ptr<Stmt> body)
        : condition_(std::move(condition)), body_(std::move(body)) {}

    std::any accept(Visitor &visitor) override {
        return visitor.visitWhileStmt(*this);
    }

    std::shared_ptr<Expr> condition_;
    std::shared_ptr<Stmt> body_;
};

} // namespace cpplox
