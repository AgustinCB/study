# HLox

Implementation of Lox (see [Crafting interpreters](https://craftinginterpreters.com/))
using Haskell.

## Grammar

```$xslt
expression      → commaExpression ;
commaExpression → ternary ( "," ternary )* ;
ternary         → equality ( "?" expression <- ternary )? ;
equality        → comparison ( ( "!=" | "==" ) comparison )* ;
comparison      → addition ( ( ">" | ">=" | "<" | "<=" ) addition )* ;
addition        → multiplication ( ( "-" | "+" ) multiplication )* ;
multiplication  → unary ( ( "/" | "*" ) unary )* ;
unary           → ( "!" | "-" ) unary
                | primary ;
primary         → NUMBER | STRING | "false" | "true" | "nil"
                | "(" expression ")" ;
```