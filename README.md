# Simple C Programs Compiler
Author: Rimsha Rizvi

## Description
This repository contains the source code for a simple compiler for a subset of the C programming language. The compiler is written in F# and uses BNF (Backus-Naur Form) formatting to define the syntax rules of the simple C language.

## Files
The compiler consists of the following components:
1. **Lexer** (`lexer.fs`): This component takes the input C program and breaks it down into a list of tokens.
2. **Parser** (`parser.fs`): This component uses BNF formatting to define the syntax rules of the simple C language. It checks if the list of tokens from the lexer conforms to these rules and returns "success" if the input program is syntactically correct, and "syntax_error" otherwise.
3. **Analyzer** (`analyzer.fs`): This component performs semantic analysis on the parsed program to check for any semantic errors.
4. **Checker** (`checker.fs`): This component checks the program for additional errors such as undefined variables, type mismatches, etc.
5. **Main File** (`main.fs`): This is the main entry point of the compiler. It coordinates the actions of the other components.
6. **Main.c** (`main1.c`,`main2.c`): These are sample input C programs for the compiler.

## Usage
1. Clone the repository to your local machine.
2. Compile and run the compiler with the sample input C program: ./main main1.c
3. If the input C program is valid, the compiler will print "success" to the console. Otherwise, it will print an error message.

If you would like to use this work for educational or other non-commercial purposes, don't hesitate to get in touch with the author for permission.
