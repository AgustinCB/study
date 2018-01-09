# Training a softmax classifier

In this lesson we will deepen our understanding of softmax classifier and we will see how to train a model that uses it.

We said that softmax is:

```
Z[L] = w[L] a[L-1] + b[L]
a[L] = e.^z[L]/sum(1, C, j, e.^z[L][j])
```

The bigest element in the Z vector will map to the biggest element in the softmax output (hence the name softmax, derived from another method called "hard max"). If C=2, softmax excentially reduces to logistic regression.

Let's take the previous picture example and say that the expected output is `[0 1 0 0]` and the neural network returns `[0.3 0.2 0.1 0.4]`. It's not performing well. The loss function that we use is: `L(yprim, y) = -sum(1, C, j, y[j] log(yprim[j]))`. Trying to make that small is the same than minimizing `-log(yprim[j])` and therefore we want `yprim[j]` to be as big as possible.

The backpropagation step for this activation function would be done by:

```
dz[L] = dJ / (dz[L]) = yprim - y
```
