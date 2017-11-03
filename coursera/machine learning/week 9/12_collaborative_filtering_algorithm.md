# Collaborative filtering algorithm

In this lesson we will cover how to build an algorithm to filter features.

## Objective

We know that we can calculate the features from the parameters and the parameters from the features.

We can take both optimization objective into one big optimization objective:

```
J(x[1:n[m]], \theta[1:n[u]]) = 1/2 sum((i,j):r(i, j), 1/2 (\theta[j]'x[i] - y(i, j)) ^ 2) + \lambda/2 sum(1, n[m], i, sum(1, n, k x[k][i] ^ 2)) + \ambda/2 sum(1, n[u], j, sum(1, n, k, \theta[k][j] ^ 2))
```

And we want to minimize this `j(x[1:n[m]], \theta[1:n[u]])`. We are now minimizing both set of parameters simultaniously!

So this is the algorithm:

1. Initialize all the parameters to small random values.
2. Minimize J using gradient descent (or an advanced optimization algorithm).
3. For a user with parameters \theta and a movie with (learned) features x, predict a star rating of \theta'x.
