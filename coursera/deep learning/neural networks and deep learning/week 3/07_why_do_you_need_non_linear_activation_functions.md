# Why do you need non-linear activation functions?

In this lesson we will explain why a neural network needs a non-linear activation function.

Given:

```
z[1] = W[1]x + b[1]
a[1] = sigmoid(z[1])
z[2] = W[2]a[1] + b[1]
a[2] = sigmoid(z[2])
```

Suppose we redifine `a` as:

```
z[1] = W[1]x + b[1]
a[1] = a[1]
z[2] = W[2]a[1] + b[1]
a[2] = a[2]
```

Then we have the following: `a[2] = w[2] (w[1]x + b[1]) + b[2] = w[2]w[2] x + w[2] b[1] + b[2]`.

Which is just a linear function of the input, which makes the hidden layers useless and it's not more expressive than logistic regression.

You may use linear activation functions in the output layers, but other than that is useless.
