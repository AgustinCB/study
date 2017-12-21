# Backpropagation intuition

In this lesson we will go over some intuition on how that computation is derived.

## Single layer

For logistic regression, we had this forward pass:

x\
w-> z = w'x+b -> a = sigmoid(z) -> l(a,y)
b/

For derivatives, we would go the other way:

                   /dw
l(a, y) -> da -> dz
                   \db

The definition for loss was: l(a,y) = -y log(a) - (1-y)log(1-a).

Now we can do:

d/da l(a,y) = -y/a + (1-y)/1-a = da

dl/dz = dl/da * da/dz
da/dz = d/dz g(z) = g'(z)
dz = da * g'(z)

dw = dz x
db = dz // For a single training example

## Two layers

x    \
W[1] -> z[1] = W[1] x + b[1] -> a[1] = sigmoid(z[1]) ->
b[1] /

W[2] \
     -> z[2] = W[2] a[1] + b[2] -> a[2] = sigmoid(z[2]) -> l(a[2], y)
b[2] /

We would use then backwards propagation to compute the derivatives:

```
dz[2] = a[2] - y
dw[2] = dz[2] a[1]'
db[2] = dz[2]
dz[1] = W[2]' * dz[2] .* g[1]'(z[1])
dw[1] = dz[1] * x'
db[1] = dz[1]
```

We'll vectorize this forumlas accross multiple examples.

```
dZ[2] = A[2] - Y
dW[2] = 1/m dZ[2]A[1]'
db[2] = np.sum(dZ[2], axis=1, keepdims=True) / m
dZ[1] = W[2]' dZ[2] .* g[1]'(Z[1])
dW[1] = 1/m dZ[1] X'
db[1] = 1/m np.sum(dZ[1], axis=1, keepdims=True)
```
