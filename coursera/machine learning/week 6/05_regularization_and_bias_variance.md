# Regularization and Bias/variance

In this lesson we will cover how regularization interacts with the bias/variance problem.

Suppose that we have the model:

```
h[\theta](x) = \theta[0] + \theta[1] x + \theta[2] x^2 + \theta[3] x^3 + \theta[4] x^4
J(\theta) = 1/(2m) sum(1, m, i, (h[\theta](x[i]) - y[i]) ^ 2) + \lambda/2m sum(1,m,j, \theta[j]^2)
```

We use regularization to try to minimize the chances of overfitting.

If we have a large \lambda, we will be penalizing all the parameters and have something close to `h[\theta](x) = \theta[0]` and will be underfitting.

If we have a very small \lambda, we may not solve the problem of high variance and end up with overfitting anyway.

It's only when we have an intermediate value of \lambda that this works.

How do we chose the right value for \lambda?

Let's suppose that we have `\lambda=0` (i.e. we don't use regularization) for all our definition of error (train, validation and test).

We would do this with the training set:

1. Try \lambda=0
2. Try 0.01
3. Try 0.02
4. Try 0.04
...
12. Try 10

This give us about 12 different models to try. Given each one of them, we can try to minize the parameters \theta for them. After that, we can use our cross validation set to pick the one that has the lowest error.

Finally, we will take the test error for that model and we will see how well our parameter will generalize.
