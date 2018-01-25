# One layer of a convolutional network

We will see now how to build one layer of a convolutional network.

We have seen how to convolve one image with two images to get an output of depth two. We will now add a bias to each output and then apply a non-linear function (ReLU for example). We'd later stack the outputs into one vector of matrixes and that would be one layer of a convolutional neural network.

So if before we had:

```
z[i] = w[i] * a[i-1] + b[i]
a[i] = g(z[i])
```

In this case, the image is `a[i-1]` and the filter is `w[i]` (the convolution is the linear operation).

Let's suppose you have 10 filters of size 3x3x3 in one layer of a neural network, how many parameters dos that layer have?

The number of parameters per image is `3x3x3+1=28` (+1 because of the bias). We have ten imagesso the total number of parameters is 280 parameters. This number of parameters don't change with the size of the input, so it's less likely to overfit.

If layer `l` is a convolutional layer, then:

- `f[l]` is a filter size.
- `p[l]` is the amount of padding.
- `s[l]` is the stride.
- `nh[l-1]` and `nw[l-1]` are the height and width of the output of the previous layer.
- `nc[l-1]` is the number of channels of the previous layer.

So we will have: `n[l] = (n[l-1] + 2p[l] - f[l])/s[l] + 1`. We also have that `nc[l]` is equal to the number of filters that we have in that layer. The size of the filter should be `f[l]xf[l]xnc[l-1]`. `a[l]` will be of dimension `nh[l]xnw[l]xnc[l]`.
