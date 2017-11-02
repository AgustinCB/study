# Anomaly detection using the multivariate Gaussian distribution

In this lesson we will cover how to apply the multivariate Gaussian distribution into a new anomaly detection algorithm.

We have two parameters, \mu and \Sigma and this function:

```
p(x;\mu, \Sigma) = 1/((2PI)^n/2 * |\Sigma|^1/2) exp(-1/2 (x - \mu)' \Sigma^-1 (x-\mu))
```

## Parameter fitting

Give a dataset of m samples, how do we define \my and \Sigma?

```
\mu = 1/m sum(1, m, i, x[i])
\Sigma = 1/m sum(1, m, (x[i] - \mu) (x[1] - \mu)')
```

## Algorithm

1. Fit the model by setting \mu and \Sigma.
2. Given a new example x, compute p(x) and flag an anomaly if p(x) < \epsilon.

## Relationship to the original model

You can prove mathematically that the relationship between them is so that the original model is an special case of the multivariate Gaussian distribution. In particular, the off-diagonal has zero values in the \Sigma matrix.

## When to use each one

You will choose the original model when you want to manually create features to capture anomalies where other features take unusual combinations of values. It's also computationally cheaper and scales better to very large number of features. The original model works ok even if m is relatively small.

You will choose this model to automatically capture correlations between different features. Although it's computationally more expensive. It must have m > n or else \Sigma is non-invertible. In practice you'd use it only if m is much bitter than n.
