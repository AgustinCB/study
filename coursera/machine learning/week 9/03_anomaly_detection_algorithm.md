# Anomaly detection algorithm

In this lesson we will cover how to apply the Gaussian distribution to apply an anomaly detection algorithm.

Lets say we have a training set with m samples, each one a vector of n dimensions. We are going to model p(x) for this dataset.

```
p(x) = p(x[1]; \mu[1], \sigma[1]^2) p(x[2]; \mu[2], \sigma[2]^2) p(x[3]; \mu[3], \sigma[3]^2) ... p(x[n]; \mu[n], \sigma[n]^2)
p(x) = prod(1, n, j, p(x[j]; \mu[j], \sigma[j]^2))
```

This is often called the density estimation.

## Algorithm

1. Choose features x[i] that you think might be indicative of anomalous examples.
2. Fit parameters \mu[1], ..., \mu[n], \sigma[1]^2, ..., \sigma[n]^2 using:

```
\mu[j] = 1/m sum(1, m, j, x[i][j])
\sigma[j]^2 = 1/m sum(1, m, j, (x[i][j] - \mu[j])^2)
```

3. Given a new example x, compute p(x) as:

```
p(x) = prod(1, n, j, p(x[j]; \mu[j], \sigma[j]^2)) = prod(1, n, j, 1 / (sqrt(2 PI) \mu[j]) exp(- (x[j] - \mu[j])^2 / 2 \sigma[j]^2))
```

If it's less than a chosen \epsilon, then it's an anomaly.
