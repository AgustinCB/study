# Gaussian Distribution

In this lesson we will cover Gaussian distribution (or normal distribution).

Say x is a real number. If x is distributed Gaussian with mean \mu and variance \sigma^2, we will write that `x ~ N(\mu, \sigma^2)`.

If you plot it, you will see that it has a bell shaped curve with the center in \mu and the width of this curve on \sigma.

This formula would look like this:

```
p(x; \mu, \sigma^2) = 1 / (sqrt(2 PI) \sigma) exp(- (x-\mu)^2 / (2\sigma^2))
```

The area under the curve should always be one.

The bigger the \sigma, the fatter and smaller the curve. The smaller the \sigma, the thiner and taller the curve.

# Parameter estimation problem

Let's talk now about the parameter estimation problem. Let's say we have a dataset of m examples where every one is a real number.

Let's say I suspect that this examples come from a gaussian distribution with some parameter \sigma and \mu but I don't know what those are. How would I estimate it?

We can say that we estimate:

```
\mu = 1/m sum(1, m, i, x[i])
\sigma^2 = 1/m sum(1, m, i, (x[i] - \mu)^2)
```

The variance is therefore the average of the distance of every example with the mean.
