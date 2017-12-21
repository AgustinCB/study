# Explanation for vectorized implementation

In this lesson we will explain in more detail the vectorized implementation of neural networks.

So we have:

```
z[1][1] = w[1] x[1] + b[1]
z[1][2] = w[1] x[2] + b[1]
z[1][3] = w[1] x[3] + b[1]
```

In this case, w[i] is some matrix and x[i] is some input vector. So the multiplication of w[1] with x[i] will also be a vector. In this case of cardinality equals to the number of rows in w[1].

If we now put all the input as columns of a matrix, the multiplication of of W with that matrix will be another matrix with `z[1][i]` as the column i (i.e. with as many columns as number examples and as many rows as the number of features). We can probe that the same apply for the A matrix very easily.
