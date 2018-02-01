# Residual Networks (ResNets) (He et al., 2015)

In this lesson we will see how to skip connections to build ResNets.

## Residual block.

Suppose you have two layers of a neural network with three units. So you have:

```
z[l+1] = W[l+1] a[l] + b[l+1]
a[l+1] = g(z[l+1])
z[l+2] = W[l+2] a[l+1] + b[l+2]
a[l+2] = g(z[l+2])
```

So for information to go from a[l] to a[l+2] we need to follow this path. A residual network takes a "shortcut" to go deeper to the neural network. To do so, we replace the last step with `a[l+2] = g(z[l+2] + a[l])`. This is also called skipped connection.

So to build a neural network you take residual blocks and stake them together to get a full network.

In practice, networks that are too deep tend to have a worse training error over time. By using residual networks, you prevent this, as it helps with banishing and exploding parameters.
