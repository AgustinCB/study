# HLox

Implementation of Lox (see [Crafting interpreters](https://craftinginterpreters.com/))
using Rust.

## Grammar

```$xslt
program         → declaration* EOF ;

declaration → classDecl
            | funDecl
	        | varDecl
            | statement ;

classDecl   → "class" IDENTIFIER "{" ( ( "class" | "setter" | "getter" )? function )* "}" ;

declWithBreak   → varDecl
                | statementWithBreak ;

statement       → exprStmt
                | forStmt
                | ifStmt
                | printStmt
                | whileStmt
                | returnStmt
                | block ;

stmtWithBreak   → exprStmt
                | forStmt
                | ifStmtWithBreak
                | printStmt
                | whileStmt
                | returnStmt
                | blockWithBreak
                | breakStmt ;

breakStmt       → break ";" ;
forStmt         → "for" "(" ( varDecl | exprStmt | ";" )
                     expression? ";"
                     expression? ")" statementWithBreak ;
block           → "{" declaration* "}" ;
blockWithBreak  → "{" declWithBreak* "}" ;
ifStmt          → "if" "(" expression ")" statement ( "else" statement )? ;
ifStmtWithBreak → "if" "(" expression ")" statementWithBreak 
                        ( "else" statementWithBreak )? ;
exprStmt        → expression ";" ;
printStmt       → "print" expression ";" ;
whileStmt       → "while" "(" expression ")" statementWithBreak ;
returnStmt      → "return" expression? ";" ;
varDecl         → "var" IDENTIFIER ( "=" expression )? ";" ;

funDecl         → "fun" function ;
function        → IDENTIFIER "(" parameters? ")" block ;
parameters      → IDENTIFIER ( "," IDENTIFIER )* ;

primary         → "true" | "false" | "nil" | "this"
                | NUMBER | STRING
                | "(" expression ")"
                | IDENTIFIER ;

expression      → assignment ;
assignment      → ( call "." )? IDENTIFIER "=" (assignment | commaExpression) ;
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
call            → primary ( "(" arguments? ")" | "." IDENTIFIER )* ;
arguments       → ternary ( "," ternary )* ;
primary         → NUMBER | STRING | "false" | "true" | "nil"
                | "fun" "(" parameters? ")" block
                | "(" expression ")"
                | ( "!=" | "==" ) equality
                | ( ">" | ">=" | "<" | "<=" ) comparison
                | ( "+" ) addition
                | ( "/" | "*" ) multiplication ;
```
