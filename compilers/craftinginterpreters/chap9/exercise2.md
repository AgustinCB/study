2. Likewise, looping can be implemented using those same tools, provided our interpreter supports an important optimization. What is it, and why is it necessary? Name a language that uses this technique for iteration.

Recursion. We will need tail call optimization to prevent the stack to overflow on large loops. Languages that use this: Haskell.
