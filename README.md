# Parser and Interpreter for a Toy Programming Language
This repository contains a series of three projects that build a parser and interpreter for a stack-oriented and functional toy programming language. The goal is to develop a progressively more powerful interpreter, supporting stack operations, lexical scoping, and functional programming.

## Project 1: Stack-Oriented Language
In the first project, we implemented a stack-oriented programming language with basic stack manipulation operations, arithmetic commands, subroutines, and dynamically scoped variables. The interpreter supports the following main functions:

parse_prog: Parses a string into a program.
eval_prog: Evaluates a parsed program with a given stack and environment.
interp: Interprets a string, executing it in the language.

## Project 2: Lexically Scoped Language
The second project introduces lexically scoped variables, allowing for better control over variable binding in recursive subroutines. This version handles local variables within subroutines, making it possible to write more complex programs without interference between recursive calls.

Key functions include:

* parse_prog: Parses a lexically scoped program.
* eval_prog: Evaluates the lexically scoped language with proper handling of variable scopes.

## Project 3: Functional Language
The third project extends the language into a functional one, adding higher-level constructs like function definitions, first-class functions, and lexical scoping. This version introduces an OCaml-like syntax and desugars high-level expressions into the stack-oriented language.

Main components:

* desugar: Converts high-level programs to low-level syntax.
* translate: Translates low-level syntax into stack-based commands.
* serialize: Serializes the stack-based program into a string for interpretation.
