# Gradient descent for neural networks

In this lesson we will how to implement gradient descent for a neural network for one hidden layer.

Parameters of the neural network: w[1], b[1], w[2], b[2]. We will have n[0] input values, n[1] hidden layers and n[2]=1 output layers. So w[1] will be of dimension (n[1], n[0]), b[1] will be a (n[1], 1), w[2] will be (n[2], n[1]) and b[2] will be (n[2], 1).

We also have a cost function `J(w[1], b[1], w[2], b[2]) = 1/m sum(1, n, i, l(a[i][2], y[i]))`.

Gradient descent would be as follow:

Random initialize parameters
Repeat until convergion {
  Compute predictions (y'[i], i=1,...,m)
  Compute dw[i] = dJ/(dw[i]), db[i] = dJ/(db[i])
  Update the parameters: W[i] = W[i] - \alpha dW[1]
                         b[i] = b[i] - \alpha db[i]
}

We already saw how to compute the predictions, we have to cover now the derivatives:

For forward propagation we have:

```
Z[i] = W[i] X + B[i]
A[i] = G[i](Z[i])
```

And A[l] will be the prediction.

For the derivatives we have to do the backpropagation step:

```
dz[2] = A[i] - Y
dw[2] = 1/m dZ[2] A[1]'
db[2] = 1/m sum(dz[2])
dz[1] = W[2]' dz[2] .* g[1]'(Z[1])
dw[1] = 1/m dz[1]X'
db[1] = 1/m sum(dz[1])
```

Where `sum(m) = np.sum(m, axis=1, keepdims=True)` in Python.
