# Backpropagation algorithm

We will cover in this lesson an algorithm to minimize the cost function:

```
J(\Theta) = - 1/m sum(1, m, i, sum(1, K, k, y[k][i] * log(h[\Theta](x[i]))[k] + (1 - y[i][k]) log(1-h[\Theta](x[i])[k]))) + \lambda / (2m) sum(1, L-1, l, sum(1, s[l], i, sum(1, s[l] + 1, j, \Theta[j][i][l] ^ 2)))
```

Our objective is to minimise `J(\Theta)` by `\Theta`. We need to compute the cost function, which is just using the formula previously listed.

We also need to calculate the partial derivative of the cost function, which is what we are going to discuss.

## Gradient computation

Let's suppose that we have only one training example, namely `(x,y)`, in the neural network described in the previous document.

1. We apply forward propagation to compute the value received:

```
a[1] = x
z[2] = \Theta[1] a[1]
a[2] = g(z(2)) (add a[2][0])
z[3] = \Theta[2] a[2]
a[3] = g(z(3)) (add a[3][0])
a[4] = \Theta[3] a[3]
a[4] = h[\Theta](x) = g(z(4))
```

Next, in order to compute the gradients, we will use an algorithm called backpropagation.

The intuition is that for each node we are going to compute `\delta[l][j] = error in the activation of node j in layer l`.

In the output layer (layer 4), for each unit `j`, we will do:

```
\delta[4][j] = a[4][j] - y[j] = h[\Theta](x)[j] - y[j]
```

If you think of each element as a vector:

```
\delta[4] = a[4] - y
```

For the hidden layers in the network:

```
\delta[3] = (\Theta[3])'\delta[4] .* g'(z[3])
\delta[2] = (\Theta[2])'\delta[3] .* g'(z[2])

g(x) = x .* (1 - x)
```

There's no delta term for the first layer, since its the input and doesn't have any error.

It's possible to probe that:

```
d / (d \Theta[l][ij]) J(\Theta) = a[l][j] \delta[l+1][i]
```

Suppose now that we have a training set of m elements.

We will do:

Set `\Delta[l][ij] = 0` (for all l, i, j).

For i = 1 to m
  Set a[i] = x[i]
  Perform forward propagation to compute a[l] for l = 2, 3, ..., L
  Using y[i], compute \delta[L] = a[L] - y [i]
  Compute \delta[L-1], \delta[L-2], ..., \delta[2]
  `\Delta[l][ij] = \Delta[l][ij] + a[l][k] \delta[l+1][i]`

```
D[l][ij] = 1/m \Delta[l][ij] + \delta \Theta[l][ij] if j != 0
D[l][ij] = 1/m \Delta[l][ij]                        if j == 0
```

Where `d/(d \Theta[l][ij]) J(\Theta) = D[l][ij]`
