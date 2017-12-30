# Gradient checking implementation notes

While implementing gradient checking keep in mind:

- Don't use gradient checking in training - only to debug. Gradient checking is very expensive.

- If algorithm fails, look at the individual components to try to identify the bug (the weights? The bias?).

- Remember your regularization term. The gradient, should also include it.

- It doesn't work with dropout (because of the previous point). If you're using, first turn it off and check with gradient checking and then turn it on and turn off gradient checking.

- Run at random initialization, your implementation may be correct when w and b are close to zero but wrong otherwise.
