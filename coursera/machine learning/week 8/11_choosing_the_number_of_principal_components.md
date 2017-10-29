# Choosing the number of principal components

In the PCA algorithm we take data as vector n dimensional and convert it into k dimensional. We will cover now how to choose k.

Useful concepts to keep in mind:

- We are trying to minimice the average squared project error.
- The total variation in the data: `1/m sum(1, m, i, ||x[i]|| ^ 2)`.

Typically, we will choose k so the smallest value so that:

```
(1/m sum(1, m, i, ||x[i] - x[i][approx]|| ^ 2)) / (1/m sum(1, m, i, ||x[i]||^2)) <= 0.01
```

i.e. 99% of variance is retained. We usually will refer to k as the variance retained rather than the number of parameters retained, because is a more meaninful metric ("we retained 99% variance" vs "we choose 100 features").

An algorithm to implement this is:

- Start with PCA with k=1
- Compute PCA
- Check if the variance is less than 0.01. If yes, do it again with k+1. If not, choose k-1.

This isn't very fast. However, `svd` give us a quantity that may help us:

```
[U, S, V] = svd(\Sigma)
```

In this case we will have S as a diagonal matrix of size nxn. And it turns out that for a given value of k, the variance for k can be computed as `1 - (sum(1, k, i, S[i][i]) / sum(1, n, i, S[i][i]))`. So we can look for the smallest value k such as that value is still bigger or equal than 0.99.

With this we will need to call the `svd` only once and to select the value of k in an efficient way from there.
