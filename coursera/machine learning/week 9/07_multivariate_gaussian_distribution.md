# Multivariate Gaussian distribution

In this lesson we will cover an extension for the anomaly detection that we developed so far.

## Motivating example

Suppose that we're monitoring machines in a data center in which we have the memory use and CPU load of the machines that are Gaussian.

Let's say that in the test example there's a point that is an outlier, but the value of the ps will be relatively high, so our anomaly detection algorithm won't catch it.

## Multivariate Gaussian (normal) distribution

Instead of model each p separately, we are going to model one p all in one go. We are going to create a vector \mu of dimension n and a matrix \Sigma of size nxn (covariance matrix).

```
p(x;\mu, \Sigma) = 1/((2PI)^n/2 * |\Sigma|^1/2) exp(-1/2 (x - \mu)' \Sigma^-1 (x-\mu))
```

Where |\Sigma| is the determinant of \Sigma (det(\Sigma)).

\mu will be a vector with the mean of each feature as items.

\Sigma is a matrix composed of the variance of the features in the main diagonal and control variables in the opposite diagonal, that decide the direction of the data and how wide the matching are should be with respect to the length. It also helps identify if two features are positively or negatively correlated.
