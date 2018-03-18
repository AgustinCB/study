# Deep RNNs

We will see now how to stack a bunch of layers together to get deeper RNNs.

Each layer will process the input of the previous one, as usual. We will then have a matrix of activation values with layers representing the rows, the activation values of each output as column and the value of the activation value at a given time in a given layer as cells. So:

```
a[l][t] = g(W[l][a][a[l][t-1], a[l-1][t]] + b[l][t])
```

Usually, we won't have more than three RNN layers, however you can add normal networks after them to make computation easier. They are usually computationally expensive to train, so it's rare to see a lot of deep recurrent layers.
