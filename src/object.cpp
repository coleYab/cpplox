#include "object.h"
#include "interpreter.h"

namespace cpplox {

std::any LoxFunction::call(Interpreter &interpreter, const std::vector<std::any> &arguments) const {
    const auto environment{std::make_shared<Environment>(closure_)};
    for (size_t i = 0; i < declaration_->params_.size(); i++) {
        environment->define(declaration_->params_[i].getLexeme(), arguments.at(i));
    }

    try {
        interpreter.executeBlock(declaration_->body_, environment);
    } catch (ReturnException &e) {
        return e.value_;
    }

    return nullptr;
}

std::shared_ptr<LoxFunction> LoxFunction::bind(std::shared_ptr<LoxInstance> instance) {
    auto environment = std::make_shared<Environment>(closure_);
    environment->define("this", std::make_any<std::shared_ptr<LoxInstance>>(instance));
    return std::make_shared<LoxFunction>(declaration_, environment);
}

std::any LoxClass::call(Interpreter &interpreter, const std::vector<std::any> &arguments) const {
    auto instance = std::make_shared<LoxInstance>(std::const_pointer_cast<LoxClass>(shared_from_this()));
    auto initializer = findMethod("init");
    if (initializer != nullptr) {
        initializer->bind(instance)->call(interpreter, arguments);
    }
    return std::make_any<std::shared_ptr<LoxInstance>>(instance);
}

} // namespace cpplox
