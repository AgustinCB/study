1. A few chapters from now, when Lox supports first-class functions and dynamic dispatch, then we technically won’t need branching statements built into the language. Show how conditional execution can be implemented in terms of those. Name a language that uses this technique for its control flow.

You could create a function such that:

```
function if(conditionFn, thenBranchFn, elseBranchFn) {
  conditionFn() ? : thenBranchFn() : elseBranchFn();
}
```

Languages like LISP do this.
