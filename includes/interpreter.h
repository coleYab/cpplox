#pragma once

#include "environment.h"
#include "error.h"
#include "expr.h"
#include "helper.h"
#include "object.h"
#include "stmt.h"

namespace cpplox {

class Interpreter : public Visitor, public Stmt::Visitor {
    EnvironmentPointer environment_;
    EnvironmentPointer globals_;
    std::map<Expr *, size_t> locals_;

    std::any evaluate(const SExpression &expr);
    void execute(const Statement &stmt);
    std::any lookUpVariable(const Token &name, Expr *expr) const;

    static std::string stringify(const std::any &value);

public:
    Interpreter();

    EnvironmentPointer &getEnvironment() { return environment_; }

    void resolve(Expr *expr, size_t depth);

    void executeBlock(const Stmts &statements, const EnvironmentPointer &environment);

    void interpret(const SExpression &expr);
    void interpret(const Stmts &stmts);

    // Expr Visitor
    std::any visitBinaryExpr(Binary &expr) override;
    std::any visitGroupingExpr(Grouping &expr) override;
    std::any visitLiteralExpr(Literal &expr) override;
    std::any visitUnaryExpr(Unary &expr) override;
    std::any visitVariableExpr(Variable &expr) override;
    std::any visitAssignExpr(Assign &expr) override;
    std::any visitLogicalExpr(Logical &expr) override;
    std::any visitCallExpr(Call &expr) override;
    std::any visitGetExpr(Get &expr) override;
    std::any visitSetExpr(Set &expr) override;
    std::any visitThisExpr(This &expr) override;
    std::any visitSuperExpr(Super &expr) override;

    // Stmt Visitor
    std::any visitSExpressionStmt(const Expression &stmt) override;
    std::any visitPrintStmt(const Print &stmt) override;
    std::any visitVarStmt(const Var &stmt) override;
    std::any visitBlockStmt(const Block &stmt) override;
    std::any visitIfStmt(const If &stmt) override;
    std::any visitFunctionStmt(const Function &stmt) override;
    std::any visitReturnStmt(const Return &stmt) override;
    std::any visitWhileStmt(const While &stmt) override;
    std::any visitClassStmt(const ClassStmt &stmt) override;
};

} // namespace cpplox
