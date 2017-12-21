# Forward and backward propagation

In this lesson we will see the details on how to implement a neural network.

## Forward propagation.

Input: a[l-1]
Output: a[l], cache(z[l])

```
A[0] = X
Z[l] = W[l] A[l-1] + B[l]
A[l] = G[l](Z[l])
```

## Backward propagation

Input: da[l]
Output: da[l-1], dW[k], db[l]

```
dz[l] = dA[l] g[l]'(Z[l])
dW[l] = 1/m dZ[l] A[l-1]'
db[l] = 1/m np.sum(dZ[l], axis=1, keepdims=True)
da[l-1] = W[l]' dZ[l]
dz[l] = W[l+1]' dZ[l+1] g[l](Z[l])
```
