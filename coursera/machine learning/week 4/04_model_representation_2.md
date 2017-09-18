# Model representation II

In this lesson, we will see how to carry our implementation of the neural network efficiently and get a good intuition on how they work and why they work.

Consider the network:

```
a[1][2] = g(\Theta[1][10] * x[0] + \Theta[1][11] * x[1] + \Theta[1][12] * x[2] + \Theta[1][13] * x[3])
a[2][2] = g(\Theta[1][20] * x[0] + \Theta[1][21] * x[1] + \Theta[1][22] * x[2] + \Theta[1][23] * x[3])
a[3][2] = g(\Theta[1][30] * x[0] + \Theta[1][31] * x[1] + \Theta[1][32] * x[2] + \Theta[1][33] * x[3])
h[\Theta](x) = a[1][3] = g(\Theta[2][10] * a[2][0] + \Theta[2][11] * a[1][2] + \Theta[2][12] * a[2][2] + \Theta[2][13] * a[2][3])
```

We will define some things:

```
a[2][1] = g(z[2][1])
a[2][2] = g(z[2][2])
a[2][3] = g(z[2][3])
```

Now we will have `z[2] = [z[2][1] z[2][2] z[2][3]]`, where `z[2]` is a three dimensional vector. Therefore:

```
z[2] = \Theta[1] x = \Theta[1] a[1]
a[2] = g(x[2])
```

Where both `z[2]` and `a[2]` are in R^3.

To take care of the bias unit, we will add `a[2][0] = 1`:

```
z[3] = \Theta[2] a[2]
h[\Theta](x) = a[3] = g(z[3])
``` 

Now, `a[2]` is in R^4.

This is called forward propagation.

## Neural network learning its own feature

Considere the previous neural network and ignore the first layer. If you just considere the last two layers, it looks a lot like logistic regression, where we're using the single node in the output layer to make a prediction `h(x) = g(\Theta[0] a[0] + \Theta[1] a[1] + \Theta[2] a[2] + \Theta[3] a[3])`, which looks roughly as the standard logistic regression model.

The difference is that the features computed by logistic regression, but using features computed by a hidden layer. And they themselves are also learned using logistic regression! Which gives you the flexibility to learn whatever feature you want to make the algorithm work.

You can have more complex neural networks adding more hidden layers to get even more interesting features for the output layer.
