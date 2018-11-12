# HLox

Implementation of Lox (see [Crafting interpreters](https://craftinginterpreters.com/))
using Haskell.

## Grammar

```$xslt
program         → declaration* EOF ;

declaration     → varDecl
                | statement ;

statement       → exprStmt
                | ifStmt
                | printStmt
                | block ;

block           → "{" declaration* "}" ;
ifStmt          → "if" "(" expression ")" statement ( "else" statement )? ;
exprStmt        → expression ";" ;
printStmt       → "print" expression ";" ;
varDecl         → "var" IDENTIFIER ( "=" expression )? ";" ;

primary         → "true" | "false" | "nil" | "this"
                | NUMBER | STRING
                | "(" expression ")"
                | IDENTIFIER ;

expression      → assignment ;
assignment      → IDENTIFIER "=" (assignment | commaExpression) ;
commaExpression → ternary ( "," ternary )* ;
ternary         → logicOr ( "?" expression <- ternary )? ;
logicOr         → logicAnd ( "or" logicAnd )* ;
logicAnd        → equality ( "and" logicAnd )* ;
equality        → comparison ( ( "!=" | "==" ) comparison )* ;
comparison      → addition ( ( ">" | ">=" | "<" | "<=" ) addition )* ;
addition        → multiplication ( ( "-" | "+" ) multiplication )* ;
multiplication  → unary ( ( "/" | "*" ) unary )* ;
unary           → ( "!" | "-" ) unary
                | primary ;
primary         → NUMBER | STRING | "false" | "true" | "nil"
                | "(" expression ")"
                | ( "!=" | "==" ) equality
                | ( ">" | ">=" | "<" | "<=" ) comparison
                | ( "+" ) addition
                | ( "/" | "*" ) multiplication ;
```