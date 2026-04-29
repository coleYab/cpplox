#pragma once

#include "expr.h"
#include <string>

namespace cpplox {

class AstPrinter : public Visitor {
public:
    std::string print(const SExpression &expr) {
        return std::any_cast<std::string>(expr->accept(*this));
    }

    std::any visitBinaryExpr(Binary &e) override {
        return parenthesize(e.op_.getLexeme(), {e.left_, e.right_});
    }

    std::any visitGroupingExpr(Grouping &e) override {
        return parenthesize("group", {e.expression_});
    }

    std::any visitLiteralExpr(Literal &e) override {
        if (!e.value_.has_value()) return std::string("nil");

        if (e.value_.type() == typeid(std::string)) {
            return std::any_cast<std::string>(e.value_);
        }

        if (e.value_.type() == typeid(double)) {
            return std::to_string(std::any_cast<double>(e.value_));
        }

        if (e.value_.type() == typeid(bool)) {
            return std::any_cast<bool>(e.value_) ? std::string("true") : std::string("false");
        }

        return std::string("?");
    }

    std::any visitUnaryExpr(Unary &e) override {
        return parenthesize(e.op_.getLexeme(), {e.right_});
    }

    std::any visitVariableExpr(Variable &) override { return std::string{"<var>"}; }
    std::any visitAssignExpr(Assign &e) override { return parenthesize("=", {std::make_shared<Literal>(e.name_.getLexeme()), e.value_}); }
    std::any visitLogicalExpr(Logical &e) override { return parenthesize(e.op_.getLexeme(), {e.left_, e.right_}); }
    std::any visitCallExpr(Call &e) override {
        std::vector<SExpression> exprs{e.callee_};
        exprs.insert(exprs.end(), e.arguments_.begin(), e.arguments_.end());
        return parenthesize("call", exprs);
    }
    std::any visitGetExpr(Get &e) override { return parenthesize(".", {e.object_, std::make_shared<Literal>(e.name_.getLexeme())}); }
    std::any visitSetExpr(Set &e) override {
        std::vector<SExpression> exprs{e.object_, std::make_shared<Literal>(e.name_.getLexeme()), e.value_};
        return parenthesize("set", exprs);
    }
    std::any visitThisExpr(This &) override { return std::string{"this"}; }
    std::any visitSuperExpr(Super &e) override { return parenthesize("super", {std::make_shared<Literal>(e.method_.getLexeme())}); }

private:
    std::string parenthesize(const std::string &name, const std::vector<SExpression> &expressions) {
        std::string result = "(" + name;
        for (const auto &expr: expressions) {
            result += " ";
            result += std::any_cast<std::string>(expr->accept(*this));
        }
        result += ")";
        return result;
    }
};

} // namespace cpplox
