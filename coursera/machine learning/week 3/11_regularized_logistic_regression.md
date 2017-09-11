# Regularized logistic regression

We will show how to addapt both techniques (gradient descent and the more advanced ones) using regularization.

We will have a cost function of this shape:

```
J(\theta) = - [1/m sum(1,m, y log(h(x)) + (1-y) log(1-h(x)))] + \lambda/2m sum(1,m,\theta^2)
```

As earlier, we will have:

```
Repeat {
  \theta0 := \theta0 - \alpha 1/m sum(1,m,(h(x) - y) * x0)
  \thetaj := \thetaj - \alpha (1/m sum(1, m, (h(x) - y) * xj) + \alpha/m \thetaj) for j = {1, ..., n}
}
```

Which is cosmetically the same than the one for linear regression, but with a different `h(x)`.

## Advanced optimization

What you need to do is to include the new regularization term into the cost function passed to the function that calculates (fminunc) the gradient descent.
