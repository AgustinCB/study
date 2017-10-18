# Optimization objective

There's one more algorithm that is very powerful and widely used in both industry in academia: The support vector machine. Compared to both regression and neural networks, it gives sometimes cleaner results on non-linear functions.

Let's cover now the optimization objective of this algorithm.

## Alternative view of logistic regression

Our logistic regression hypothesis was:

```
h[\theta](x) = 1 / (1 + \epsilon^(-\theta'x))
z = \theta' x
```

If y=1, we want h[\theta](x) to be close to 1 (i.e. z>>0).
If y=0, we want h[\theta](x) to be close to 0 (i.e. z<<0).

The cost function of logistic regression for every pair input-output (x,y) will contribute with this value:

```
-(y log(h[\theta](x)) + (1-y) log(1-h[\theta](x))) = -y log(1 / (1 + e ^ (z))) - (1 - y) log(1 - 1 /(1 + e^(-z)))
```

If y=1 (want z >> 0), then only the first term matters, so we use the term `-log(1 / (1 + e ^ (z)))`. If we plot that, we find that when z is large, we get a very small value. When it's small, it get's a very large value.

In the support vector machine, instead of using that function, we will use a simplification:

```
f(x) = 0 if x >= 1
       1 - x if x < 1
```

The second part of that function has slop 1, but that really doesn't matter that much. We can use almost any alternative, as long as it is a decreasing straight line. This methods give us some computational advantages.

In the case of y=0, we use the term: `-log(1-1/(1+e^-z))`. Similar to the other one, but inverted. As before, for the support vector machine, we are going to use a different function:

```
f'(x) = 0 if x <= -1
        -1 + x if x > -1
```

The first function is called cost[1](z) and the second one is cost[0](z).

With this, we can have a different cost function than the one on logistic regression:

```
J[\theta](x) = 1/m sum(1, m, i, y[i] * cost[1](\theta'x[i]) + (1-y[i]) cost[0](\theta'x[i]) + \lambda/2m sum(1, n, i, \theta[i] ^ 2))
```

By convention, we write it a little bit differently:

```
J[\theta](x) = C A[\theta](x) + B(\theta)
A[\theta](x) = sum(1, m, i, y[i] * cost[1](\theta'x[i]) + (1-y[i]) cost[0](\theta'x[i]))
B(\theta) = sum(1, n, i, \theta[i] ^ 2) / 2
```

Where C works in a similar but inverse way to \lambda. As usual, we have to minimize this J function.

This function don't give a probability but either 1 or 0.
