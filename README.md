## cxlox

This is a C implementation of the xlox language. There are two banches, `main` and `xlox`. The `main` branch is used for the `Crafting Interpreters` implementation of the lox language. The `xlox` branch is used for the implementation of the xlox language.

## Grammar

```
program       := declaration*
declaration   := varDecl
               | funDecl
               | letDecl
               | classDecl
               | importDecl
               | statement

varDecl       := "var" IDENTIFIER typehint? ["=" expression] ";"
funDecl       := "fun" IDENTIFIER "(" [IDENTIFIER typehint? ("," IDENTIFIER typehint?)*] ")" returnhint? "{" declaration* "}"
letDecl       := "let" IDENTIFIER typehint? "=" expression ";"
classDecl     := "class" IDENTIFIER ["<-" IDENTIFIER] "{" methodDecl "}"
methodDecl    := IDENT "(" [IDENTIFIER typehint? ("," IDENTIFIER typehint?)*] ")" "{" declaration* "}"
importDecl    := "include" IDENTIFIER ";"

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
               | delStmt

exprStmt      := expr ";"
printStmt     := "print" expr ";"
blockStmt     := "{" declaration* "}"
ifStmt        := "if" "(" expr ")" statement ["else" statement]
whileStmt     := "while" "(" expr ")" statement
forStmt       := "for" "(" [expr | varDecl] ";" expr? ";" expr? ")" statement
returnStmt    := "return" expr? ";"
breakStmt     := "break" ";"
continueStmt  := "continue" ";"
switchStmt    := "switch" "(" expr ")" "{" switchCase* defaultCase? "}"
switchCase    := "case" addition ":" statement*
defaultCase   := "default" ":" statement*
delStmt       := "del" propAccess ("," propAccess)* ";"

expr          := comma
comma         := optional ("," optional)*
assignment    := IDENTIFIER ("=" | "+=" | "-=" | "*=" | "/=") assignment
               | optional
optional      := conditional [":" optional]
conditional   := equality ["?" equality]
equality      := comparison (("==" | "!=") comparison)*
comparison    := addition (("<" | ">" | "<=" | ">=") addition)*
addition      := multiply  (("+" | "-") multiply)*
multiply 	    := unary (("*" | "/") unary)*
unary         := ("!" | "-") call
               | call
call          := primary [ "(" [expr ("," expr)*)] ")" 
                         | ("." | "?.") expr 
                         | "[" [expr ("," expr)*)] "]" ]
primary       := NUMBER
               | STRING
               | IDENTIFIER [(',' IDENTIFIER)* '=>' (expr | blockStmt)]
               | "true"
               | "false"
               | "nil"
               | "this"
               | "super" "." expr
               | "(" expr ")"
               | "[" expr ("," expr)* "]"
NUMBER        := [0-9]+ ["." [0-9]+]
STRING        := "\"" (CHARACTER)* "\""
IDENTIFIER    := [a-zA-Z_][a-zA-Z0-9_]*

typehint      := ":" IDENTIFIER
returnhint    := "->" IDENTIFIER
propAccess    := IDENT (("." | ".?") IDENT)+
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
- Properties can be deleted from instances using the `del` keyword followed by a single property access.
  - Multiple properties can be specified using a comma as a delimiter.
- Properties can be accessed in a safe way with the `?.` operator.
  - If the left operand is not an instance, the result will be `nil`.
  - If the left operand is an instance and the right operand is a property that instance has, the result will be that property. Otherwise, `nil` will be returned.
- `include` can be used to run another `.lox` or `.xlox` file and add the results of that file into the current VM
  - A guard system is on the todo list
- Lists can be created from literals
  - Lists can be indexed and proper bounds checking is performed.

## Todo
- Assert statement
  - `assert EXPR : 'MSG'`
- Guard system from including the same file multiple times
  - Ideally not a statement if possible

## Build

To make this project, you need to install the following dependencies:

- CMake >= 3.10

To create the executable, run the following commands:

```bash

```
