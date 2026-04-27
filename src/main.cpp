#include "error.h"
#include "interpreter.h"
#include "parser.h"
#include "resolver.h"
#include "scanner.h"
#include <filesystem>
#include <fstream>
#include <iostream>
#include <string_view>

namespace cpplox {

void run(const std::string_view &code) {
    Scanner scanner{code};
    const Tokens tokens{scanner.scan()};
    if (hasError_) {
        exit(2);
    }

    Parser parser{tokens};
    const auto stmts = parser.parse();

    const auto interpreter = std::make_shared<Interpreter>();
    Resolver resolver{interpreter};

    resolver.resolve(stmts);

    interpreter->interpret(stmts);
}

void runFile(const std::string &fileName) {
    std::ifstream infile{fileName};

    if (!infile.is_open()) {
        throw std::runtime_error("could not open file " + fileName);
    }

    const std::string sCode{std::istreambuf_iterator<char>(infile), std::istreambuf_iterator<char>()};
    run(sCode);
    hasError_ = false;
}

void runPrompt() {
    while (true) {
        std::cout << ">>> ";
        std::string prompt;
        std::getline(std::cin, prompt);
        if (prompt.empty()) break;
        run(prompt);
    }
}

} // namespace cpplox

int main(const int argv, char **argc) {
    using namespace cpplox;

    if (argv == 2) {
        const std::string fileName{argc[1]};
        runFile(fileName);
    } else if (argv == 1) {
        runPrompt();
    } else {
        std::cout << "Usage cpplox [script]" << std::endl;
        exit(1);
    }
}
