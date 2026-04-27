#pragma once

#include "error.h"
#include "expr.h"
#include "interpreter.h"
#include "stmt.h"
#include <map>
#include <vector>

namespace cpplox {

enum class FunctionType {
    NONE,
    FUNCTION,
    METHOD,
    INITIALIZER
};

enum class ClassType {
    NONE,
    CLASS,
    SUBCLASS
};

using SInterpreter = std::shared_ptr<Interpreter>;

class Resolver : public Visitor, public Stmt::Visitor {
    SInterpreter interpreter_;
    std::vector<std::map<std::string, bool>> scopes_;
    FunctionType currentFunction = FunctionType::NONE;
    ClassType currentClass = ClassType::NONE;

    void beginScope();
    void endScope();

    void declare(const Token &name);
    void define(const Token &name);

    void resolveLocal(Expr &expr, const Token &name) const;
    void resolveFunction(const Function &stmt, FunctionType functionType);

public:
    void resolve(const Stmts &stmts);
    void resolve(const Statement &stmt);
    void resolve(const SExpression &expr);
    explicit Resolver(SInterpreter interpreter);

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
