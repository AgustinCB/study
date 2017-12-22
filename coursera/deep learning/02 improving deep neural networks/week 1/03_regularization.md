# Regularization

In this lesson, we will see how to reduce the variance of a neural network by using regularization.

For logistic regression, we try to minimize this function:

```
J(w,b) = 1/m sum(1, m, i, L(ý[i], y[i])) + \lambda/2m w' w
```

We could also regularice the parameter b, but it isn't often needed. In this case \lambda is the regularization parameter. This is called L2 regularization.

You could also have L1 regularization which adds the the following term instead: `\lambda/2m sum(1, nx, i, |w|)`. This will make w be sparce and be a little bit more efficient.

The regulzarization parameter is considered a hyperparameter.

For neural networks, we have `J(w[1], b[1], ..., w[L], b[L]) = 1/m sum(1,m,i, L(ý[i],y[i])) + \lambda/2m sum(1, L, l, ||w[l]||^2)`. In this case, `||w[l]||^2` is called frobenius norm of w[l].

With this extra term, we have to modify slightly the backwards propagation algorithm:

```
dw[l] = (from backprop) + \lambda/m w[l]
w[l] = w[l] - \alpha dw[l]
```

This is why L2 regularization is often called weight decay.
