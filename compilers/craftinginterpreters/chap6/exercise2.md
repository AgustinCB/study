The grammar would be something like:

```
expression <- ternary
ternary <- equality ( "?" expression <- ternary )? ;
```

So the left operand has higher precedence than the others and the one in the middle has lowest.
