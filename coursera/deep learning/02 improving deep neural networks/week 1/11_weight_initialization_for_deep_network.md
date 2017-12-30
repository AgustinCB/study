# Weight initialization for deep networks

In this lesson we will see how to partially solve the problem of exploding and vanishing gradients by carefully initialize weights.

Let's assume a single neuron with for features as input and one output y.

```
z = w[1] x[1] + w[2] x[2] + w[3] x[3] + w[4] x[4]
```

The larger n (the number of features), the smaller we want w[i] to be. One way would be to say:

```
var(w[i]) = 1/n (or 2/n)
```

You can do that by setting `w[l]` to `np.random.randn(shape) * np.sqrt(1/n[l-1])`. The second term of the multiplication sets the variance to 1/n. If you use 2/n[l-1] it will set it to 2/n. The second is better for ReLU activation functions and the first for the tanh activation function.

This will maintain the output always on a similar scale and it prevents weights from exploding or vanishing too quickly.
