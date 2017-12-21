# Forward propagation in a deep network

In this lesson we will cover how to implement forward propagation in a deep network.

Suppose we have a neural network with three inputs, two hidden layers of five units, one hidden layer of three units and one output.

We will start with a single training example x:

```
a[0] = x
z[1] = w[1] a[0] + b[1]
a[1] = g[1](z[1])
z[2] = w[2] a[1] + b[2]
a[2] = g[2](z[2])
...
z[4] = w[4] a[3] + b[1]
a[4] = g[4](z[4])
```

Where g is the vector of activation functions. Ingeneral:

```
z[l] = w[l] a[l-1] + b[l]
a[l] = g[l](z[l])
```

Where 1 < l <= L.

Vectorized:

```
A[0] = X
Z[1] = W[1] A[0] + B[1]
A[1] = g[1](Z[1])
Z[2] = W[2] A[1] + B[2]
A[2] = g[2](Z[2])
Y' = A[4] = g[4](Z[4])
```

And generalized:

```
Z[L] = W[L] A[L-1] + B[L]
A[L] = g[L](Z[L])
```

There will be a for loop for 1..L. But there's no way to implement this without this for loop.
