# cpplox - C++ interpreter implementation for lox language

## Introduction

`cpplox` is a C++23 implementation of an tree walker interpreter for the Lox programming language. cpplox is a dynamically typed language as described in Crafting Interpreters book by Robert Nystrom.

The project implements a tree-walk interpreter with a complete pipeline, lexical analysis and parsing into an abstract syntax tree (AST). It also has runtime evaluation using the Visitor pattern.

This repository is intended as an educational and experimental implementation of language runtime concepts using modern C++.

## Features
- [x] Implemented

- [x] Interactive REPL

- [x] Script execution from file

- [x] Scanner (lexer) with error reporting

- [x] Recursive-descent parser

- [x] Abstract Syntax Tree (AST)

- [x] Visitor-based evaluation

- [x] Tree-walk interpreter

- [x] Dynamic typing (std::any)

- [x] Variable declarations and assignments

- [x] Lexical scoping with nested environments

- [x] Arithmetic, comparison, and logical operators

- [x] print statements

- [x] Single-line (//) and multi-line (/* */) comments

## TODO
- [ ] Control flow (if, while, for)

- [ ] Functions and closures

- [ ] Classes and inheritance

- [ ] Static resolver pass

- [ ] Detailed runtime error diagnostics

## Example

```lox
var x = 10;
var y = 20;
print x + y;

{
  var x = "Hello ";
  print x + "World";
}

```

```terminaloutput
30
Hello World
```

## Architecture Overview
### Scanner

The scanner converts source code into a sequence of tokens.
It supports literals, identifiers, keywords, operators, and comments, while tracking line numbers for diagnostics.

### Parser

The parser is a recursive-descent parser that builds an AST based on Lox’s expression grammar.
Parsing errors use panic-mode recovery to continue parsing subsequent statements.

### AST

Expressions and statements are represented as class hierarchies and traversed using the Visitor pattern.

### Interpreter

The interpreter evaluates the AST directly (tree-walk).
Values are dynamically typed using std::any, and truthiness follows Lox semantics.

### Environment

Lexical scoping is implemented using chained environments. Variable lookup walks enclosing scopes at runtime.

##  How to run it?

### Requirements

- C++23-compatible compiler (GCC 13+, Clang 16+)

- make

- POSIX-like environment (Linux, macOS, WSL)

### Build

```make
make build
```

The build command will generate a binary located at `build/cpplox`.

### Run

To run the interactive repl use make like the following.
```make
make run
```

or you can also run it directly.
```bash
./build/cpplox
```

If you want to give a full file to the interpreter you can just simply run.

```bash
./build/cpplox <file to the program>
```

## Design Decisions

- Tree-walk interpreter for clarity and simplicity

- Visitor pattern for AST traversal and extensibility

- std::any used to model dynamic typing

- Emphasis on correctness and readability over performance

## Known Limitations

- Runtime error handling is incomplete

- Block scoping implementation is still changing

- No static analysis or resolver pass

- No garbage collection


## Roadmap

- [ ] Implement control flow statements

- [ ] Add functions and closures

- [ ] Replace std::any with std::variant

- [ ] Improve runtime diagnostics

- [ ] Optional bytecode VM backend

## References

Crafting Interpreters — Robert Nystrom - https://craftinginterpreters.com

