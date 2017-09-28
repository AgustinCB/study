# Backpropagation intuition

In this lesson, we will try to develop the intuition about what backpropagation is doing by going through the mechanical steps of backpropagation.

Let's take an example: A neural network with two input units, two intermediate layers of two nodes and one output node.

When we do forward propagation, we get the values in the input value to generate `a[2][1]` and `a[2][2]`, which (along side with a bias unit) are used to generate `a[3][1]` and `a[3][2]`, which are used (alongside with the bias unit) to generate the output `a[4][1]`. The especific calculus of the activation unit of layer `l`, depends on an intermediate step called `z[l]`. That step is calculated by: `z[l] = \Theta[2] * a[l]'`.

Backpropagation does something very similar, but instead of going from left to right, we go from right to left. To understand it better, let's look at the const function:

```
J(\Theta) = - 1/m sum(1, m, i, sum(1, K, k, y[k][i] * log(h[\Theta](x[i]))[k] + (1 - y[i][k]) log(1-h[\Theta](x[i])[k]))) + \lambda / (2m) sum(1, L-1, l, sum(1, s[l], i, sum(1, s[l] + 1, j, \Theta[j][i][l] ^ 2)))
```

Let's consider a single example, the case of 1 output unit and ignoring regularization (`\lambda = 1`).

```
cost(i) = y[i] log[\Theta](x[i]) + (1-y[i]) log[\Theta](x[i])
```

Think of `cost(i)` as an approximation to `(h[\Theta](x[i]) - y[i])^2`, the error function. I.e. is a way to know how well is the network doing on example `i`.

Backpropagation will compute `\delta[l][j]`, as the error of cost for `a[l][j]`. Formally, is the derivative of the cost function on `z[l][j]`. They are a measure of how much we would like to change the configuration of the neural network.

For the output layer, we set the delta value as the difference of the output and the expected value. For the intermediate values, we go layer by layer from right to left, calculating `\delta` based on the previous results, using:

```
\delta[l][j] = \Theta[l] * \delta[l+1]'
```

We are ignoring the bias unit, however there are some implementations that actually use them.
