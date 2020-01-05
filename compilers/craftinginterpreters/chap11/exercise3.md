3.- Python and JavaScript allow you to freely access an object’s fields from outside of its own methods. Ruby and Smalltalk encapsulate instance state. Only methods on the class can access the raw fields, and it is up to the class to decide which state is exposed. Most statically typed languages offer modifiers like private and public to control which parts of a class are externally accessible on a per-member basis.

What are the trade-offs between these approaches and why might a language prefer one or the other?

The trade-offs are mostly compile time encapsulation. By using modifiers to private and public, you are making usages of those properties outside the scope they were thought of explicit errors, making sure that your objects as they were intended and making it easier in the future, for example, to change the internal mechanisms of the class without forcing the clients to change any code (i.e. leaving the interface as is).

Not enforcing that, on the other hand, gives more flexiblity to the programmer and keeps the language simple.
