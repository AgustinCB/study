# Why ResNets work

In this lesson we will go through one example of ResNets to see why the work so well.

We saw that making a network too deep may hurt the learning algorithm.

Let's say that we have a big NN that outputs `a[l]` and we modify the neural network to add a couple extra layers using ReLU after `a[l]`, outputing `a[l+2]`. If we add ResNet block and use `a[l]` to calculate `a[l+2]`, we have:

```
a[l+2] = g(z[l+2] + a[l]) = g(w[l+2] * a[l+1] + b[l+2] + a[l])
```

If `w[l+2]=0` and `b[l+2]=0`, we have that `a[l+2] = g(a[l]) = a[l]`. Which means that learning the identity function is easy for a residual blcok to learn, so it's able to perform at least as well as the layers before. It also makes easy from gradient descent to arrive to a point in which it doesn't hurt performance and then improve from there.

On images that don't use same convolution, you may need some parameters. `W[s]`, multiplying `a[l]` to make sure that the dimensions make sense. Same happens with the Pool layers.
