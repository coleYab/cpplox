#include "interpreter.h"
#include <chrono>

namespace cpplox {

Interpreter::Interpreter() {
    environment_ = std::make_shared<Environment>();
    globals_ = std::make_shared<Environment>();

    globals_->defineIf(
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

    globals_->defineIf(
        "exit",
        std::make_shared<NativeFunction>(
            0,
            [](Interpreter &, const std::vector<std::any> &) -> double {
                exit(0);
                return 0;
            }
        )
    );
}

std::any Interpreter::evaluate(const SExpression &expr) {
    return expr->accept(*this);
}

void Interpreter::execute(const Statement &stmt) {
    stmt->accept(*this);
}

std::any Interpreter::lookUpVariable(const Token &name, Expr *expr) const {
    if (!locals_.contains(expr)) {
        return globals_->get(name.getLexeme());
    }

    const size_t distance = locals_.at(expr);
    return environment_->getAt(distance, name.getLexeme());
}

std::string Interpreter::stringify(const std::any &value) {
    if (Helper::isNull(value)) return "nil";
    if (Helper::isNumber(value)) return Helper::toString(value);
    if (Helper::isBoolean(value)) return Helper::toString(value);
    if (Helper::isString(value)) return Helper::toString(value);
    try {
        auto instance = std::any_cast<std::shared_ptr<LoxInstance>>(value);
        return instance->toString();
    } catch (const std::bad_any_cast &) {
    }
    try {
        auto callable = std::any_cast<std::shared_ptr<LoxCallable>>(value);
        return "<fn>";
    } catch (const std::bad_any_cast &) {
    }
    try {
        auto klass = std::any_cast<std::shared_ptr<LoxClass>>(value);
        return klass->getName();
    } catch (const std::bad_any_cast &) {
    }
    return "?";
}

void Interpreter::resolve(Expr *expr, const size_t depth) {
    locals_[expr] = depth;
}

void Interpreter::executeBlock(const Stmts &statements, const EnvironmentPointer &environment) {
    const auto previous = this->environment_;
    this->environment_ = environment;
    try {
        for (const auto &statement: statements) {
            execute(statement);
        }
    } catch (ReturnException &) {
        this->environment_ = previous;
        throw;
    } catch (std::exception &e) {
        std::cout << "Error while executing block statement " << e.what() << std::endl;
    }

    this->environment_ = previous;
}

void Interpreter::interpret(const SExpression &expr) {
    try {
        const auto value = evaluate(expr);
        std::cout << "Value is " << stringify(value) << std::endl;
    } catch (std::exception &e) {
        std::cout << "Some error happened" << e.what() << std::endl;
    }
}

void Interpreter::interpret(const Stmts &stmts) {
    try {
        for (const auto &stmt: stmts) {
            execute(stmt);
        }
    } catch (std::exception &e) {
        std::cerr << "ERROR: " << e.what() << std::endl;
    }
    globals_->clear();
    environment_->clear();
}

// --- Expr Visitor ---

std::any Interpreter::visitLiteralExpr(Literal &expr) {
    return expr.value_;
}

std::any Interpreter::visitGroupingExpr(Grouping &expr) {
    return evaluate(expr.expression_);
}

std::any Interpreter::visitUnaryExpr(Unary &expr) {
    const std::any right = evaluate(expr.right_);

    switch (expr.op_.getType()) {
        case TokenType::MINUS:
            if (!Helper::isNumber(right)) throw RuntimeError{};
            return -Helper::getNumber(right);
        case TokenType::BANG: {
            return !Helper::isTruthy(right);
        }
        default:
            return nullptr;
    }
}

std::any Interpreter::visitBinaryExpr(Binary &expr) {
    const std::any left = evaluate(expr.left_);
    const std::any right = evaluate(expr.right_);

    switch (expr.op_.getType()) {
        case TokenType::MINUS:
            return Helper::getNumber(left) - Helper::getNumber(right);
        case TokenType::PLUS:
            if (Helper::isString(left) && Helper::isString(right)) {
                return Helper::getString(left) + Helper::getString(right);
            }
            if (Helper::isNumber(left) && Helper::isNumber(right)) {
                return Helper::getNumber(left) + Helper::getNumber(right);
            }
            throw RuntimeError{};
        case TokenType::STAR:
            return Helper::getNumber(left) * Helper::getNumber(right);
        case TokenType::SLASH:
            return Helper::getNumber(left) / Helper::getNumber(right);
        case TokenType::BANG_EQUAL:
            return !Helper::isEqual(left, right);
        case TokenType::EQUAL_EQUAL:
            return Helper::isEqual(left, right);
        case TokenType::GREATER:
            return Helper::isGreater(left, right);
        case TokenType::LESS:
            return Helper::isLess(left, right);
        case TokenType::GREATER_EQUAL:
            return Helper::isGreaterEqual(left, right);
        case TokenType::LESS_EQUAL:
            return Helper::isLessEqual(left, right);
        default:
            return nullptr;
    }
}

std::any Interpreter::visitVariableExpr(Variable &expr) {
    return lookUpVariable(expr.name_, &expr);
}

std::any Interpreter::visitAssignExpr(Assign &expr) {
    std::any value = evaluate(expr.value_);

    if (!locals_.contains(&expr)) {
        globals_->assign(expr.name_.getLexeme(), value);
        return value;
    }

    const size_t dist = locals_[&expr];
    environment_->assignAt(dist, expr.name_.getLexeme(), value);
    return value;
}

std::any Interpreter::visitLogicalExpr(Logical &expr) {
    std::any left = evaluate(expr.left_);

    if (expr.op_.getType() == TokenType::AND) {
        if (!Helper::isTruthy(left)) return left;
    } else {
        if (Helper::isTruthy(left)) return left;
    }

    return evaluate(expr.right_);
}

std::any Interpreter::visitCallExpr(Call &expr) {
    const std::any callee = evaluate(expr.callee_);

    std::vector<std::any> args;
    for (const auto &arg: expr.arguments_) {
        args.push_back(evaluate(arg));
    }

    try {
        const auto function = std::any_cast<std::shared_ptr<LoxCallable>>(callee);
        if (args.size() != function->arity()) {
            throw std::runtime_error{"function arguments and function parameters count must match!"};
        }
        return function->call(*this, args);
    } catch (const std::bad_any_cast &) {
        try {
            const auto fun = std::any_cast<std::shared_ptr<NativeFunction>>(callee);
            return fun->call(*this, args);
        } catch (const std::bad_any_cast &) {
            const auto klass = std::any_cast<std::shared_ptr<LoxClass>>(callee);
            if (args.size() != klass->arity()) {
                throw std::runtime_error{"function arguments and function parameters count must match!"};
            }
            return klass->call(*this, args);
        }
    }
}

std::any Interpreter::visitGetExpr(Get &expr) {
    std::any object = evaluate(expr.object_);
    try {
        auto instance = std::any_cast<std::shared_ptr<LoxInstance>>(object);
        return instance->get(expr.name_);
    } catch (const std::bad_any_cast &) {
        throw RuntimeError{};
    }
}

std::any Interpreter::visitSetExpr(Set &expr) {
    std::any object = evaluate(expr.object_);
    try {
        auto instance = std::any_cast<std::shared_ptr<LoxInstance>>(object);
        std::any value = evaluate(expr.value_);
        instance->set(expr.name_, value);
        return value;
    } catch (const std::bad_any_cast &) {
        throw RuntimeError{};
    }
}

std::any Interpreter::visitThisExpr(This &expr) {
    return lookUpVariable(expr.keyword_, &expr);
}

std::any Interpreter::visitSuperExpr(Super &expr) {
    size_t distance = locals_[&expr];
    auto superclass = std::any_cast<std::shared_ptr<LoxClass>>(environment_->getAt(distance, "super"));
    auto object = std::any_cast<std::shared_ptr<LoxInstance>>(environment_->getAt(distance - 1, "this"));

    auto method = superclass->findMethod(expr.method_.getLexeme());
    if (method == nullptr) {
        throw RuntimeError{};
    }

    return std::make_any<std::shared_ptr<LoxCallable>>(method->bind(object));
}

// --- Stmt Visitor ---

std::any Interpreter::visitSExpressionStmt(const Expression &stmt) {
    evaluate(stmt.expression_);
    return nullptr;
}

std::any Interpreter::visitPrintStmt(const Print &stmt) {
    const std::any value = evaluate(stmt.expression_);
    std::cout << stringify(value) << std::endl;
    return nullptr;
}

std::any Interpreter::visitVarStmt(const Var &stmt) {
    std::any value = nullptr;
    if (stmt.initializer_ != nullptr) {
        value = evaluate(stmt.initializer_);
    }

    environment_->define(stmt.name_.getLexeme(), value);
    return nullptr;
}

std::any Interpreter::visitBlockStmt(const Block &stmt) {
    executeBlock(stmt.statements_, std::make_shared<Environment>(environment_));
    return nullptr;
}

std::any Interpreter::visitIfStmt(const If &stmt) {
    if (Helper::isTruthy(evaluate(stmt.condition_))) {
        execute(stmt.thenBranch_);
    } else if (stmt.elseBranch_ != nullptr) {
        execute(stmt.elseBranch_);
    }

    return nullptr;
}

std::any Interpreter::visitFunctionStmt(const Function &stmt) {
    auto decl = std::make_shared<Function>(stmt);
    const auto function = std::make_shared<LoxFunction>(decl, environment_);

    std::shared_ptr<LoxCallable> callable = function;
    environment_->define(stmt.name_.getLexeme(), std::make_any<std::shared_ptr<LoxCallable>>(callable));

    return nullptr;
}

std::any Interpreter::visitReturnStmt(const Return &stmt) {
    std::any value = nullptr;
    if (!Helper::isNull(stmt.value_)) value = evaluate(stmt.value_);
    throw ReturnException{value};
}

std::any Interpreter::visitWhileStmt(const While &stmt) {
    while (Helper::isTruthy(evaluate(stmt.condition_))) {
        execute(stmt.body_);
    }

    return nullptr;
}

std::any Interpreter::visitClassStmt(const ClassStmt &stmt) {
    std::shared_ptr<LoxClass> superclass = nullptr;
    std::shared_ptr<Environment> previousEnv = nullptr;

    if (stmt.superclass_ != nullptr) {
        auto superVal = evaluate(stmt.superclass_);
        try {
            auto callable = std::any_cast<std::shared_ptr<LoxCallable>>(superVal);
            superclass = std::dynamic_pointer_cast<LoxClass>(callable);
        } catch (const std::bad_any_cast &) {
            throw RuntimeError{};
        }
        if (superclass == nullptr) {
            throw RuntimeError{};
        }
    }

    environment_->define(stmt.name_.getLexeme(), nullptr);

    if (stmt.superclass_ != nullptr) {
        previousEnv = environment_;
        environment_ = std::make_shared<Environment>(previousEnv);
        environment_->define("super", std::make_any<std::shared_ptr<LoxClass>>(superclass));
    }

    std::map<std::string, std::shared_ptr<LoxFunction>> methods;
    for (const auto &method: stmt.methods_) {
        auto function = std::make_shared<LoxFunction>(method, environment_);
        methods[method->name_.getLexeme()] = function;
    }

    auto klass = std::make_shared<LoxClass>(stmt.name_.getLexeme(), superclass, methods);

    if (stmt.superclass_ != nullptr) {
        environment_ = previousEnv;
    }

    environment_->assign(stmt.name_.getLexeme(), std::make_any<std::shared_ptr<LoxCallable>>(klass));
    return nullptr;
}

} // namespace cpplox
