# Bidirectional RNN

We will see how to use bidirectional RNN, which let you at one point of time, take a look at previous and forward information.

Supposing an arbitrary block, a bidirectional RNN works as follow:

We will basically have two sets of `a` vectors: One that starts at the first value and goes to the end and another one that starts at the end and goes to the beginning. Later, it uses both of those activation values to make the prediction.

Usually, doing something like:

```
y'[t] = g(W[y][a[f][t], a[b][t]] + b[y])
```

This allows a given block to take information from present, past and future.
