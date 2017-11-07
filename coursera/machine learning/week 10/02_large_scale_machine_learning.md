# Large scale machine learning

For our algorithms, we came up with a cost function and then use gradient descent to minimize. For big datasets, this is very expensive. We will now see a modification that is more efficient.

## Linear regression with gradient descent

```
h[\theta](x) = sum(1, n, j, \theta[j] x[j])
J[train](\theta) = 1/2m sum(1, m, i, (h[\theta](x[i]) - y[i]) ^ 2)
```

And gradient descent would be:

```
Repeat {
  \theta[j] := \theta[j] - \alpha 1/m sum(1, m, i, (h[\theta](x[i]) - y[i]) x[j][i])
}
```

This will slowly take the parameters to the global minimum. This is called batch gradient descent.

If m is large, computing the sum term can be very expensive.

## Stochastic gradient descent

We will modify slightly the cost function:

```
cost(\theta, (x[i], y[i])) = 1/2 (h[\theta](x[i] - y[i]) ^ 2)
J[train](\theta) = 1/m sum(1, m, i, cost(\theta, (x[i], y[i])))
```

Now let's see a different algorithm that uses this:

1. Randomly shuffle the dataset.
2. Repeat until convergion {
    for i in 1:m {
      for j in 1:n {
        \theta[j] = \theta[j] - \alpha (h[\theta](x[i] - y[i] * x[j][i]))
      }
    }
  }

So it try to fit the parameter one example at a time.

It's like batch gradient descent, but making progress on every example instead of scanning all parameters to get a progress on the parameters.
