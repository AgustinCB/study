3. Is this program valid?

```
fun scope(a) {
  var a = "local";
}
```

In other words, are a function’s parameters in the same scope as its local variables, or in an outer scope? What does Lox do? What about other languages you are familiar with? What do you think a language should do?

It should error. Shouldn't be allowed to define two variables in the same scope. And yeahm they are in the same scope.

Some languages would interpret that as "shadowing" a variable. If the language is statically typed, it would allow you to "change the type" of the variable.

