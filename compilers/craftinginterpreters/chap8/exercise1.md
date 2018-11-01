1. The REPL no longer supports entering a single expression and automatically printing its result value. That’s a drag. Add support to the REPL to let users type in both statements and expressions. If they enter a statement, execute it. If they enter an expression, evaluate it and display the result value.

Won't do it. It's a bad idea. If you want to type expression, simply add a `print` as a prefix and `;` as a suffix. Not so hard.
