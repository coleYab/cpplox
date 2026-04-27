#pragma once

#include <any>
#include <map>
#include <memory>
#include <stdexcept>
#include <string>

namespace cpplox {

class Environment : public std::enable_shared_from_this<Environment> {
    std::shared_ptr<Environment> enclosing_;
    std::map<std::string, std::any> values_;

public:
    explicit Environment(std::shared_ptr<Environment> enclosing = nullptr)
        : enclosing_(std::move(enclosing)) {}

    void define(const std::string &name, const std::any &value) {
        if (values_.contains(name)) {
            throw std::runtime_error{"variable with this name is already defined"};
        }
        values_[name] = value;
    }

    void defineIf(const std::string &name, const std::any &value) {
        try {
            define(name, value);
        } catch (...) {
        }
    }

    [[nodiscard]] std::any get(const std::string &name) const {
        if (values_.contains(name)) {
            return values_.at(name);
        }

        if (enclosing_ != nullptr) {
            return enclosing_->get(name);
        }

        throw std::runtime_error{"failed to get variable: " + name};
    }

    void assign(const std::string &name, const std::any &value) {
        if (values_.contains(name)) {
            values_[name] = value;
            return;
        }

        if (enclosing_ != nullptr) {
            enclosing_->assign(name, value);
            return;
        }

        throw std::runtime_error{"failed to get variable: " + name};
    }

    void clear() { values_.clear(); }

    std::any getAt(const size_t distance, const std::string &name) {
        auto env = this;
        for (size_t i = 0; i < distance; i++) {
            env = env->enclosing_.get();
        }
        return env->get(name);
    }

    void assignAt(const size_t distance, const std::string &name, const std::any &value) {
        auto env = this;
        for (size_t i = 0; i < distance; i++) {
            env = env->enclosing_.get();
        }
        env->assign(name, value);
    }
};

using EnvironmentPointer = std::shared_ptr<Environment>;

} // namespace cpplox
