# Building blocks of deep neural networks

In this lesson we will see how to put all the components together to create a deep neural network.

Suppose a neural network with three hidden layer of four units.

For layer l, you will have w[l] and b[l] and doing forward propagation will grab the input a[l-1] and output a[l] using:

```
z[l] = w[l] a[l-1] + b[l]
a[l] = g[l](z[l])
```

We will cache z[l] for backwards propagation, where the input will be da[l] and our cached z[l] and the output will be da[l-a], dw[l] and db[l].
