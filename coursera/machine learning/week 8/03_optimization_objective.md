# Optimization objective

In unsupervised algorithm, we also have a cost function that we try to minimize. In this lesson we will cover which one it's and how to use it, so we learn how to debug it and how to optimize it.

## K-means optimization objective

`c[i]` will indicate the cluster of the example `x[i]` and `\mu[k]` is the cluster centroid of k.

```
J(c, \mu) = 1/m * sum(1, m, i, || x[i] - \mu[c[i]] || ^ 2)
```

I.e. we want to minimize the sum of the difference of each example with cluster centroid to which it has been assigned. This is often called the distortion of the algorithm.

The first step of the algotithm, setting the examples on the clusters, minimizes the function J with respect to c while holding the centroid fixed.

The second step minimizes the function J with respect to \mu.

To decide whether we do another iteration or not, we check the value J after doing so.
