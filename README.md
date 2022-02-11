## cxlox

This is a C implementation of the xlox language. There are two banches, `main` and `xlox`. The `main` branch is used for the `Crafting Interpreters` implementation of the lox language. The `xlox` branch is used for the implementation of the xlox language.

## Grammar

```
program       := declaration*
declaration   := varDecl
               | funDecl
               | letDecl
               | statement
varDecl       := "var" IDENTIFIER ["=" expression] ";"
funDecl       := "fun" IDENTIFIER "(" [IDENTIFIER ("," IDENTIFIER)*] ")" "{" declaration* "}"
letDecl       := "let" IDENTIFIER "=" expression ";"
statement     := exprStmt
               | printStmt
               | blockStmt
               | ifStmt
               | whileStmt
               | forStmt
               | returnStmt
               | breakStmt
               | continueStmt
               | switchStmt
exprStmt      := expr ";"
printStmt     := "print" expr ";"
blockStmt     := "{" declaration* "}"
ifStmt        := "if" "(" expr ")" statement ["else" statement]
whileStmt     := "while" "(" expr ")" statement
forStmt       := "for" "(" (expr | varDecl)? ";" expr? ";" expr? ")" statement
returnStmt    := "return" expr? ";"
breakStmt     := "break" ";"
continueStmt  := "continue" ";"
switchStmt    := "switch" "(" expr ")" "{" switchCase* defaultCase? "}"
switchCase    := "case" conditional ":" statement*
defaultCase   := "default" ":" statement*
expr          := comma
               | "{" [assignment ("," assignment)*] "|" declaration* "}" 
               | "{" [assignment ("," assignment)*] "->" expression "}" 
comma         := optional ("," optional)*
assignment    := IDENTIFIER ("=" | "+=" | "-=" | "*=" | "/=") assignment
               | optional
optional      := conditional (":" optional)?
conditional   := equality ("?" equality)?
equality      := comparison (("==" | "!=") comparison)*
comparison    := addition (("<" | ">" | "<=" | ">=") addition)*
addition      := multiply  (("+" | "-") multiply)*
multiply 	    := unary (("*" | "/") unary)*
unary         := ( "!" | "-") unary
               | call
call          := primary ("(" [expr ("," expr)*)] ")")*
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

## Details

- The ternary expression is simulated through 2 binary expressions, the conditional ("?") and the optional (":") operators.
  - The conditional operator is evaluated first. If the left operand is truthy, the right operand is returned. Otherwise, nil is returned.
  - The optional operator is evaluated second. If the left operand equal to nil, the right operand is returned. Otherwise, the first is returned.
  - Inspired by [this blog post](https://dev.to/mortoray/we-dont-need-a-ternary-operator-309n)
- Constants can be created using the `let` keyword.
  - Prevents alterations to any globals or locals declared with the keyword.
- Loops can use `break` and `continue` keywords to alter control flow.
- Switch cases pass through one another, allowing multiple matches for one statement group.
  - Can be prevented by use of `break` statement.
  - To allow colons in the `case` syntax, the case matches start at the 'conditional' level.
  - Default case, `default`, is available for when a match does not occur.

## Build

To make this project, you need to install the following dependencies:

- CMake >= 3.10

To create the executable, run the following commands:

```bash

```
