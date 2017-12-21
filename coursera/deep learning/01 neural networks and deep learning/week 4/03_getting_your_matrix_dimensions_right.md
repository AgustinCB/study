# Getting your matrix dimensions right

In this lesson we will cover how to check the dimensions of the matrix we're working with as a debugging tool.

Syppose we have a five layers network with four hidden layers of size three, five, four and two and one output layer of one.

So have that:

```
z[i] = w[i] a[i-1] + b[i]
```

We also have that `n = [2 3 5 4 2 1]`, as the vector dimension.

With this, z[1] would be a (n[1], 1) dimensional matrix. a[0] would be (n[0], 1). So want a matrix that multiplied by (n[0], 1) gives us a (n[1], 1) matrix: A (3,2) matrix (or (n[1],n[0]) more generally).

Even more generally:

size(w[l]) = (n[l], n[l-1])

Similarly, the vector[l] has to be size (n[l], 1).

The derivatives have to have the same dimension as the original matrixes.

## Vectorized implementation

So we have now:

```
Z[i] = W[i] * A[i-1] + B[i]
```

Now size(Z[i]) = (n[i], m) and size(A[i]) = (n[i], m) and size(B[i]) = (n[i],m). The dimensions of W[i] stays the same.
