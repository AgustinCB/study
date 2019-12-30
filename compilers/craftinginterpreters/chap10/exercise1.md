1. Our interpreter carefully checks that the number of arguments passed to a function matches the number of parameters it expects. Since this check is done at runtime on every call, it has a real performance cost. Smalltalk implementations don’t have that problem. Why not?

Because each method with multiple arguments has it's argument names as part of the name and when you call it, you have to name those arguments.
