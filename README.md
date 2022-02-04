## cxlox

This is a C implementation of the xlox language. There are two banches, `main` and `xlox`. The `main` branch is used for the `Crafting Interpreters` implementation of the lox language. The `xlox` branch is used for the implementation of the xlox language.

## Grammar

```
program       := declaration*
declaration   := varDecl
               | statement
varDecl       := "var" IDENTIFIER ["=" expression] ";"
statement     := exprStmt
               | printStmt
							 | blockStmt
exprStmt      := expr ";"
printStmt     := "print" expr ";"
blockStmt     := "{" declaration* "}"
expr          := comma
comma				  := optional ("," optional)*
assignment    := IDENTIFIER "=" assignment
               | optional
optional      := conditional (":" optional)?
conditional   := equality ("?" equality)?
equality      := comparison (("==" | "!=") comparison)*
comparison    := addition (("<" | ">" | "<=" | ">=") addition)*
addition      := multiply  (("+" | "-") multiply )*
multiply 	    := unary (("*" | "/") unary)*
unary         := ( "!" | "-") unary
               | primary
primary       := NUMBER
               | STRING
							 | IDENTIFIER
							 | "true"
							 | "false"
							 | "nil"
							 | "(" expr ")"
NUMBER        := [0-9]+ ("." [0-9]+)?
STRING				:= "\"" (CHARACTER)* "\""
IDENTIFIER    := [a-zA-Z_][a-zA-Z0-9_]*
```

## Details

- The ternary expression is simulated through 2 binary expressions, the conditional ("?") and the optional (":") operators.
  - The conditional operator is evaluated first. If the left operand is truthy, the right operand is returned. Otherwise, nil is returned.
  - The optional operator is evaluated second. If the left operand equal to nil, the right operand is returned. Otherwise, the first is returned.
  - Inspired by [this blog post](https://dev.to/mortoray/we-dont-need-a-ternary-operator-309n)

## Build

To make this project, you need to install the following dependencies:

- CMake >= 3.10

To create the executable, run the following commands:

```bash

```
