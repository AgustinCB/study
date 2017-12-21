# Vectorizing across multiple examples

In this lesson we will how to vectorize our way to compute the output of a neural network.

We had previously:

```
z[1] = W[1]x + b[1]
a[1] = sigmoid(z[1])
z[2] = W[2]a[1] + b[1]
a[2] = sigmoid(z[2])
```

So for every training example we have that `a[2][i] = y'[1]`. If we want to compute this for all our training examples, we would have to make a for loop to compute all those equations.

To vectorize our computation, we will start by defining a matrix X as all our training examples stacked as columns. Then, we would do:

```
Z[1] = W[1] X + B[1]
A[1] = sigmoid(Z[1])
Z[2] = W[2] A[1] + B[2]
A[2] = sigmoid(Z[2])
```

Where Z[i], A[i] and B[i] are now matrixes formed by stacking the vectors of each training set in corresponding to that particular variable, similarly to what we did with the X matrix.
