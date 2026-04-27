#pragma once

#include <iostream>
#include <string>
#include <exception>

namespace cpplox {

inline bool hasError_{false};

inline void report(const size_t line, const std::string &where, const std::string &message) {
    std::cerr << "[" << line << "]: ERROR " << where << ": " << message << std::endl;
}

inline void error(const size_t line, const std::string &message) {
    report(line, "", message);
    hasError_ = true;
}

class RuntimeError : public std::exception {};

class CompilationError : public std::exception {};

} // namespace cpplox
