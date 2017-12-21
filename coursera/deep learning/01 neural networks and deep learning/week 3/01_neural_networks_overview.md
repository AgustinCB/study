# Neural networks overview

In this lesson we will see a quick overview of how to implement a neural network.

We saw how this model:

```
x[1] \
x[2] - Unit -> y' = a
x[3] /
```

Which maps to this computation graph:

```
x \
w - z = w' x + b -> a = sigmoid(z) -> l(a, y)
b /
```

You can even form neural networks by stacking different neurons. For example, if you have four layers (input, layer 1, layer 2 and output), you will have:

```
z[1] = W[1] x + b[1]
a[1] = sigmoid(z[1])
z[2] = W[2] a[1] + b[1]
a[2] = y' = sigmoid(z[2])
l = l(a[2], y)
```

Where W is a matrix of weights.
