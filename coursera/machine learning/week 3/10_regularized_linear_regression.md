# Regularized linear regression

For linear regression, we had previously worked in two algorithms: One using gradient descent and other using the normal equation. In this lesson we will take them and generalize them in the terms of regularize linear regression.

As we previously discussed, our new cost function is:

```
J(\theta) = 1/2m sum(1, m, (h(x) - y)^2) + \lambda sum(1,n,\theta^2)
```

Previously, we were using gradient descent for the previous cost function. With this new cost function, we will have something like this:

```
Repeat {
  \theta0 := \theta0 - \alpha 1/m sum(1,m,(h(x) - y) * x0)
  \thetaj := \thetaj - \alpha (1/m sum(1, m, (h(x) - y) * xj) + \alpha/m \thetaj) for j = {1, ..., n}
}
```

We have then a regularized gradient descent function, when the second term of each option is the derivative of the cost functions.

Another way to write it is:

```
Repeat {
  \theta0 := \theta0 - \alpha 1/m sum(1,m,(h(x) - y) * x0)
  \thetaj := \thetaj (1 - \alpha \lambda/m) - \alpha 1/m sum(1, m, (h(x) - y) * xj) for j = {1, ..., n}
}
```

The term `(1 - \alpha \lambda /m)` has an interesting effect: It will be slightly less than one. The effect then is that \thetaj will be shrinked by this term. The second term, on the other hand, will be exact same term that what we were using in the previous cases.

## Normal equation

With the normal equation:

```
X = [x1' ... xm']'; y = [y1...ym]'
```

We want to minimize `J(\theta)`, where:

```
\theta = (X'X)^-1 X' y
```

Using regularization this formula changes as follow:

```
\theta = (X'X + \lambda A)^-1 X' y
```

Where A is an identity matrix of size n with a zero in the element (0,0).

## Non-invertibility

Suppose `m <= n` (examples vs features). Then `X'X` will be non-invertible or singular. You can still use a pseudoinverse. However, for \lambda > 0, it is possible to prove that `X'X + \lambda A` is invertible, taking care of any possible problem related with that.
