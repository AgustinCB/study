# Attention model

In this lesson we will see the exact details of how to implement attention model.

Let's say that we have an input sentence and that we use a bidirectional network to translate it. We have also a forward RNN to generate the translation. For simplicity we will call the activation values there `s` instead of `a`. THey will take as input `C` which is calculated from the outputs of the bidirectional network multiplied by certain attention weights `\alpha`. We will have that `sum(1, n, t, \alpha[1][t]) = 1` and `c[i] = sum(1, n, t, \alpha[i, t] * a[t])`. In here, `\alpha[t][t']` is the amount of attention `y[t]` should pay to `a[t']`. To make sure that `\alpha` sums one:

```
\alpha[t][t'] exp(e[t][t']) / sum(1, Tx, t', exp(e[t][t']))
```

How do we compute `e[t][t']`? One way is to create a simple neural network using `s[t-1]` and `a[t']` as the other input.

One problem of this algorithm is that it takes quadratic time.

This idea have been applied to other problems such as image captioning (Xu et. al., 2015).
