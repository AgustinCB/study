# Cost function

In this lesson we will start talking about an algorithm for fitting the parameters of a neural network given a training set.

We will begin for the cost function.

We'll focus on an application for classification.

We will suppose three inputs in the layer 1, a five nodes second layer,  a five nodes second layer and a four nodes output layer. `L` will be the number of layers in the network and `s[l]` the number of units (not counting bias unit) in layer `l`.

We are considering two cases of classification:

- Binary classification, with one output unit (`s[l] = 1`) whose output will be `h[\Theta](x)` and is a real number and `y` will either be one or zero.

- Multi-class classification (K classes). In this case, `y` will be a K-dimensional vector in which every element is either zero of one.

## Define the cost function

For logistic regression we use:

```
J(\theta) = - 1/m sum(a, m, i, y[i] * log(h[\theta](x[i])) + (1 - y[i]) * log(1 - h[\theta](x[i]))) + \lambda / (2m) * sum(1, n, i, \theta[i] ^ 2)
```

For neural networks, we will use a generalization of this, where instead of having on logist compression, we will have K (where K = 1 if it's a binary problem).

We will say that `h[\Theta](x)` is a K-dimension vector and `(h[\Theta](x))[i] = y[i]`. With that, our new cost function will be:

```
J(\Theta) = - 1/m sum(1, m, i, sum(1, K, k, y[k][i] * log(h[\Theta](x[i]))[k] + (1 - y[i][k]) log(1-h[\Theta](x[i])[k]))) + \lambda / (2m) sum(1, L-1, l, sum(1, s[l], i, sum(1, s[l] + 1, j, \Theta[j][i][l] ^ 2)))
```

The main difference is that we are summing the output of every output unit in the main term.
