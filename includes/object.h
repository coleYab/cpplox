#pragma once

#include "environment.h"
#include "error.h"
#include "stmt.h"
#include <exception>
#include <functional>
#include <map>

namespace cpplox {

class Interpreter;
class LoxInstance;

class ReturnException : public std::exception {
public:
    std::any value_;

    explicit ReturnException(std::any value) : value_(std::move(value)) {}
};

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
        : arity_(arity), fn_(std::move(fn)) {}

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
    EnvironmentPointer closure_;
    std::shared_ptr<Function> declaration_;

public:
    explicit LoxFunction(std::shared_ptr<Function> func, EnvironmentPointer closure)
        : closure_(std::move(closure)), declaration_(std::move(func)) {}

    std::any call(Interpreter &interpreter, const std::vector<std::any> &arguments) const override;

    [[nodiscard]] size_t arity() const override {
        return declaration_->params_.size();
    }

    std::shared_ptr<LoxFunction> bind(std::shared_ptr<LoxInstance> instance);
};

class LoxClass : public LoxCallable, public std::enable_shared_from_this<LoxClass> {
    std::string name_;
    std::shared_ptr<LoxClass> superclass_;
    std::map<std::string, std::shared_ptr<LoxFunction>> methods_;

public:
    LoxClass(std::string name, std::shared_ptr<LoxClass> superclass,
             std::map<std::string, std::shared_ptr<LoxFunction>> methods)
        : name_(std::move(name)), superclass_(std::move(superclass)), methods_(std::move(methods)) {}

    std::any call(Interpreter &interpreter, const std::vector<std::any> &arguments) const override;

    [[nodiscard]] size_t arity() const override {
        auto initializer = findMethod("init");
        if (initializer != nullptr) return initializer->arity();
        return 0;
    }

    std::shared_ptr<LoxFunction> findMethod(const std::string &name) const {
        if (methods_.contains(name)) return methods_.at(name);
        if (superclass_ != nullptr) return superclass_->findMethod(name);
        return nullptr;
    }

    const std::string &getName() const { return name_; }
};

class LoxInstance : public std::enable_shared_from_this<LoxInstance> {
    std::shared_ptr<LoxClass> class_;
    std::map<std::string, std::any> fields_;

public:
    explicit LoxInstance(std::shared_ptr<LoxClass> klass) : class_(std::move(klass)) {}

    std::any get(const Token &name) {
        if (fields_.contains(name.getLexeme())) {
            return fields_[name.getLexeme()];
        }

        auto method = class_->findMethod(name.getLexeme());
        if (method != nullptr) {
            return std::make_any<std::shared_ptr<LoxCallable>>(method->bind(shared_from_this()));
        }

        throw RuntimeError{};
    }

    void set(const Token &name, const std::any &value) {
        fields_[name.getLexeme()] = value;
    }

    std::string toString() const {
        return class_->getName() + " instance";
    }
};

} // namespace cpplox
