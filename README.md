## cxlox

This is a C implementation of the xlox language. There are two banches, `main` and `xlox`. The `main` branch is used for the `Crafting Interpreters` implementation of the lox language. The `xlox` branch is used for the implementation of the xlox language.

## Grammar

```
program       := declaration*
declaration   := varDecl
               | funDecl
               | statement
varDecl       := "var" IDENTIFIER ["=" expression] ";"
funDecl       := "fun" IDENTIFIER "(" [IDENTIFIER ("," IDENTIFIER)*] ")" "{" declaration* "}"
statement     := exprStmt
               | printStmt
               | blockStmt
               | ifStmt
               | whileStmt
               | forStmt
               | returnStmt
exprStmt      := expr ";"
printStmt     := "print" expr ";"
blockStmt     := "{" declaration* "}"
ifStmt        := "if" "(" expr ")" statement ["else" statement]
whileStmt     := "while" "(" expr ")" statement
forStmt       := "for" "(" (expr | varDecl)? ";" expr? ";" expr? ")" statement
returnStmt    := "return" expr? ";"
expr          := assignment
assignment    := IDENTIFIER "=" assignment
               | equality
equality      := comparison (("==" | "!=") comparison)*
comparison    := addition (("<" | ">" | "<=" | ">=") addition)*
addition      := multiply  (("+" | "-") multiply)*
multiply 	    := unary (("*" | "/") unary)*
unary         := ( "!" | "-") unary
               | call
call          := primary ["(" [expr ("," expr)*)] ")"]
primary       := NUMBER
               | STRING
               | IDENTIFIER
               | "true"
               | "false"
               | "nil"
               | "(" expr ")"
NUMBER        := [0-9]+ ("." [0-9]+)?
STRING        := "\"" (CHARACTER)* "\""
IDENTIFIER    := [a-zA-Z_][a-zA-Z0-9_]*
```


## Build

To make this project, you need to install the following dependencies:

- CMake >= 3.10

To create the executable, run the following commands:

```bash

```
