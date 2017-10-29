# Reconstruction from compressed representation

In this lesson, we will cover how to uncompress data compressed by the PCA algorithm.

So with PCA we converted data from m dimensional to k dimensional, where k <= m. How can we do the opposite? Can we map from an array of vectors on R^k to an array of vectors on R^m?

We can do something like this:

```
x[approx] = U[reduce] * z
```

This will create a vector `x[approx]` that will be close to the original value `x`, with the particularity that the values `x[approx]` will have a projection error of zero.
