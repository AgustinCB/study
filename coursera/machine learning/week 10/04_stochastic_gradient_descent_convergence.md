# Stochastic gradient descent convergence

In this less we will cover how to know that the stochastic gradient descent and how to tune the learning parameter \alpha.

## Checking for convergence

With batch gradient descent, we would plot the error as a function of a number of iterations.

This wouldn't work in stochastic gradient descent, but we can try something else:

```
cost(\theta, (x[i], y[i])) = 1/2 (h[\theta](x[i] - y[i]))^2
```

During learning, we will compute cost(\theta, (x[i], y[i])) for updating \theta. Then every 1000 iterations (for example), we will plot this cost averaged over the last 1000 examples processed by the algorithm. The bigger the iterations, the lesser the noice but the bigger the steps you have to wait to get feedback.

## Learning rate

A usual technique is to have \alpha as a constant and slowly decrease it over time if we want \theta to converge. Usually using: `\alpha = C1 / (iteration_number + C2)`. However, this approach needs two new constants instead of one.
