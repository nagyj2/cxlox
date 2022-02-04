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
expr          := assignment
assignment    := IDENTIFIER "=" assignment
               | equality
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


## Build

To make this project, you need to install the following dependencies:

- CMake >= 3.10

To create the executable, run the following commands:

```bash

```
