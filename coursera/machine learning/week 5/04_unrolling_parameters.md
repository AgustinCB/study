# Unrolling parameters

In this lesson we will cover an implementation detail to unroll parameters while implementing the backpropagation algorithm.

The implementation that we were creating worked fine when were using logistic or linear regression, but because the initial parameters and the gradient expected from the cost functions are meant to be vectors, while we will have groups of matrices instead, they won't work as well for neural networks.

What we will do is "unroll" our matrices into vectores.

This can be done by this way:

```
\Theta = [\Theta1[:]; \Theta2[:]; \Theta3[:]]
```

You can use the reshape command to get a vector and return a matrix.

We will use this method to roll and unroll matrices in the cost function, the gradient function and when passing the initial theta.
