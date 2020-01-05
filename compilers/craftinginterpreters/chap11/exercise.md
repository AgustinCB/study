1. Why is it safe to eagerly define the variable bound to a function’s name when other variables must wait until after they are initialized before they can be used?

Because the code of the definition of a function, its body, is not executed till the function is called and at that point it is safe to reference its own name.
