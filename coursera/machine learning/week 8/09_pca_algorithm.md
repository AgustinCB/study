# PCA algorithm

In this lesson we will cover the implementation of the PCA algorithm.

## Data preprocessing

Before applying PCA, we should always preprocess the data as follows:

Given the training set x[1], x[2], ..., x[m], we perform feature scaling and mean normalization. So given:

```
u[j] = 1/m sum(1, m, i, x[i][j])
```

We replace each x[i][j] with x[i][j] - u[j], so each feature has 0 mean. If we have differente features on differente scales, scale them to have comparable range of values.

After that, we perform PCA, where we want to get K vectors that minimises the projection error for each points.

The mathematical proof is far more complicated than the procedure to get the data. Said procedure is as follow:

Supposing that we want to reduce the data from n-dimensional to k-dimensional.

We compute then the covariance matrix:

```
\Sigma = 1/m sum(1, n, i, (x[i]) (x[i])')
```

Where \Sigma is a nxn matrix. Compute eigenvectors of matrix \Sigma:


```
[U, S, V] = svd(\Sigma)
```

Where svd means singular value decomposition, a way to compute the eigenvectors. You can use eig also.

This outputs three matrixes. We will focus in the U matrix, also a nxn matrix, where each column is the values that we want. From there, we will take the first k vectors and that will give us the direction onto which we want to project the data.

Once we have that into a new matrix called `U[reduce]` (that will be nxk). Now we will say that `z = U[reduce]' x`, where x will be a training example and the result will be a matrix z of size kx1 with the projection.

So:

After mean normalization and feature scaling, we calculate the \Sigma function, get the first k eigenvectors and use that to calculate z for the input x.
