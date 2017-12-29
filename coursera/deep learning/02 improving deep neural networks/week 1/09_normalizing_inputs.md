# Normalizing inputs

Suppose a training set with two input features, x[1] and x[2]. There's two steps to normalize the inputs of the training set:

```
\mu = 1/m sum(1, m, i, x[i])
\sigma2 = 1/m sum(a, m, i, x[i] ** 2)
x -= \mu
x /= \sigma2
```

That makes the variance of x[1] and x[2] be equals to one and the mean to be zero. You should use the same \mu and \sigma2 for the test set.

## Why normalize the features?

If you use unnormalized features and your features are in widely different ranges, your gradient descent will need a very small learning rate to find its way to the minimum.
