```
expr → expr ( "(" ( expr ( "," expr )* )? ")" | "." IDENTIFIER )*
     | IDENTIFIER
     | NUMBER
```

This grammar is for function invocation.

```
expr -> expr call
expr -> IDENTIFIER
expr -> NUMBER

call -> "." IDENTIFIER
call -> "(" ")"
call -> "(" args ")"

args -> expr
args -> args "," expr
```
