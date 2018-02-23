# Backpropagation through time

In this lesson we will see how backpropagation works in a recurrent neural network.

For forward propagation, what we do is:

```
a[i] = g(W[a][a] a[i-1] + W[a][x] x[i] + b[a])
y[i] = g(W[y][a] a[i] + b[y])
```

Now we need a loss function. For a single element, we have `l(y', y) = y log(y') - (1 - y) log(1 - y')`. And the overall loss: `L(y', y) = sum(1, Ty, t, l(y'[t], y[t]))`.

Backpropagation would work similarly, but backwards: It starts in the last sample and it calculates the intermediate values one by one till it arrives to the first one.
