#include "resolver.h"

namespace cpplox {

Resolver::Resolver(SInterpreter interpreter) : interpreter_(std::move(interpreter)) {
    scopes_.emplace_back();
}

void Resolver::resolve(const Stmts &stmts) {
    for (const auto &stmt: stmts) {
        resolve(stmt);
    }
}

void Resolver::resolve(const Statement &stmt) {
    if (stmt == nullptr) return;
    stmt->accept(*this);
}

void Resolver::resolve(const SExpression &expr) {
    expr->accept(*this);
}

void Resolver::beginScope() {
    scopes_.emplace_back();
}

void Resolver::endScope() {
    scopes_.pop_back();
}

void Resolver::declare(const Token &name) {
    if (scopes_.empty()) return;
    auto &scope = scopes_.back();
    if (scope.contains(name.getLexeme())) {
        error(name.getLine(), "Already declared a variable with this name.");
    }
    scope[name.getLexeme()] = false;
}

void Resolver::define(const Token &name) {
    if (scopes_.empty()) return;
    scopes_.back()[name.getLexeme()] = true;
}

void Resolver::resolveLocal(Expr &expr, const Token &name) const {
    for (int i = static_cast<int>(scopes_.size() - 1); i >= 0; i--) {
        if (scopes_[static_cast<size_t>(i)].contains(name.getLexeme())) {
            interpreter_->resolve(&expr, scopes_.size() - 1 - static_cast<size_t>(i));
            return;
        }
    }
}

void Resolver::resolveFunction(const Function &stmt, FunctionType functionType) {
    auto enclosing = currentFunction;
    currentFunction = functionType;
    beginScope();

    for (const Token &param: stmt.params_) {
        declare(param);
        define(param);
    }

    resolve(stmt.body_);

    endScope();
    currentFunction = enclosing;
}

// --- Expr Visitor ---

std::any Resolver::visitBinaryExpr(Binary &expr) {
    resolve(expr.left_);
    resolve(expr.right_);
    return nullptr;
}

std::any Resolver::visitGroupingExpr(Grouping &expr) {
    resolve(expr.expression_);
    return nullptr;
}

std::any Resolver::visitLiteralExpr(Literal &) {
    return nullptr;
}

std::any Resolver::visitUnaryExpr(Unary &expr) {
    resolve(expr.right_);
    return nullptr;
}

std::any Resolver::visitVariableExpr(Variable &expr) {
    const std::string &name = expr.name_.getLexeme();
    const bool isNotInitialized = !scopes_.empty()
                                  && scopes_.back().contains(name)
                                  && scopes_.back()[name] == false;
    if (isNotInitialized) {
        error(expr.name_.getLine(), "Can't read local variable in its own initializer");
    }

    resolveLocal(expr, expr.name_);
    return nullptr;
}

std::any Resolver::visitAssignExpr(Assign &expr) {
    resolve(expr.value_);
    resolveLocal(expr, expr.name_);
    return nullptr;
}

std::any Resolver::visitLogicalExpr(Logical &expr) {
    resolve(expr.left_);
    resolve(expr.right_);
    return nullptr;
}

std::any Resolver::visitCallExpr(Call &expr) {
    resolve(expr.callee_);

    for (const auto &arg: expr.arguments_) {
        resolve(arg);
    }

    return nullptr;
}

std::any Resolver::visitGetExpr(Get &expr) {
    resolve(expr.object_);
    return nullptr;
}

std::any Resolver::visitSetExpr(Set &expr) {
    resolve(expr.value_);
    resolve(expr.object_);
    return nullptr;
}

std::any Resolver::visitThisExpr(This &expr) {
    if (currentClass == ClassType::NONE) {
        error(expr.keyword_.getLine(), "Can't use 'this' outside of a class.");
        return nullptr;
    }
    resolveLocal(expr, expr.keyword_);
    return nullptr;
}

std::any Resolver::visitSuperExpr(Super &expr) {
    if (currentClass == ClassType::NONE) {
        error(expr.keyword_.getLine(), "Can't use 'super' outside of a class.");
    } else if (currentClass != ClassType::SUBCLASS) {
        error(expr.keyword_.getLine(), "Can't use 'super' in a class with no superclass.");
    }
    resolveLocal(expr, expr.keyword_);
    return nullptr;
}

// --- Stmt Visitor ---

std::any Resolver::visitSExpressionStmt(const Expression &stmt) {
    resolve(stmt.expression_);
    return nullptr;
}

std::any Resolver::visitPrintStmt(const Print &stmt) {
    resolve(stmt.expression_);
    return nullptr;
}

std::any Resolver::visitVarStmt(const Var &stmt) {
    declare(stmt.name_);

    if (stmt.initializer_ != nullptr) {
        resolve(stmt.initializer_);
    }

    define(stmt.name_);
    return nullptr;
}

std::any Resolver::visitBlockStmt(const Block &stmt) {
    beginScope();
    resolve(stmt.statements_);
    endScope();
    return nullptr;
}

std::any Resolver::visitIfStmt(const If &stmt) {
    resolve(stmt.condition_);
    resolve(stmt.thenBranch_);

    if (stmt.elseBranch_ != nullptr) {
        resolve(stmt.elseBranch_);
    }

    return nullptr;
}

std::any Resolver::visitFunctionStmt(const Function &stmt) {
    declare(stmt.name_);
    define(stmt.name_);

    resolveFunction(stmt, FunctionType::FUNCTION);

    return nullptr;
}

std::any Resolver::visitReturnStmt(const Return &stmt) {
    if (currentFunction == FunctionType::NONE) {
        error(stmt.keyword_.getLine(), "Can't return from a top level function");
        return nullptr;
    }

    if (currentFunction == FunctionType::INITIALIZER) {
        if (stmt.value_ != nullptr) {
            error(stmt.keyword_.getLine(), "Can't return a value from an initializer.");
        }
        return nullptr;
    }

    if (stmt.value_ != nullptr) {
        resolve(stmt.value_);
    }

    return nullptr;
}

std::any Resolver::visitWhileStmt(const While &stmt) {
    resolve(stmt.condition_);
    resolve(stmt.body_);
    return nullptr;
}

std::any Resolver::visitClassStmt(const ClassStmt &stmt) {
    ClassType enclosingClass = currentClass;
    currentClass = ClassType::CLASS;

    declare(stmt.name_);
    define(stmt.name_);

    if (stmt.superclass_ != nullptr) {
        resolve(stmt.superclass_);
        beginScope();
        scopes_.back()["super"] = true;
        currentClass = ClassType::SUBCLASS;
    }

    beginScope();
    scopes_.back()["this"] = true;

    for (const auto &method: stmt.methods_) {
        FunctionType declaration = FunctionType::METHOD;
        if (method->name_.getLexeme() == "init") {
            declaration = FunctionType::INITIALIZER;
        }
        resolveFunction(*method, declaration);
    }

    endScope(); // this

    if (stmt.superclass_ != nullptr) {
        endScope(); // super
    }

    currentClass = enclosingClass;
    return nullptr;
}

} // namespace cpplox
