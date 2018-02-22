# Recurrent neural network model

In this lesson we will see how to build a model for recurrent neural network.

We could create an standard neural network, but this usually don't work well:

- Inputs and outputs can be different lengths in different examples.
- It doesn't share features learned across different positions of text.

Using a better representation will also let us reduce the number of parameters, given that the size of the parameters would be about dictionary.size * phrase.size.

A recurrent neural network will take a word, pass it to a layer and try to predict if it's a word or not. Then, in the next step, instead of just working with the new word, it'd also accept the activation value from the previous sample. The same for every sample. This layer assumes Tx = Ty. a[0] would be usually initialised to zero (or randomly). There're some parameters: Input would be multiplied by `W[a][x]`, the output by `W[y][a]` and the activation value with `W[a][a]`.

One problem of this layer is that it only uses information from the previous words. Biderectional recurrent neural networks try to solve this problem.

So we have:

```
a[i] = g(W[a][a] a[i-1] + W[a][x] x[i] + b[a]) = g(w[a][a[t-1], x[t]],b[a])
y[i] = g(W[y][a] a[i] + b[y])
```

Usually for `a[i]` we use tanh or ReLU and for `y[i]` we use sigmoid.

In the second part, we defined `W[a]` as a matrix formed by stacking `W[a][a]` and `W[a][x]` together horizontally.
