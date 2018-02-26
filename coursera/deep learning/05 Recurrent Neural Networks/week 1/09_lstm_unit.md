# LSTM (long short term memory) unit

In this lesson we will see how to use the LSTM unit, a more powerful solution to the vanishing problme than the GRU unit.

We had:

```
C'[t] = tanh(W[c][\Gamma[r] * C[t-1], x[t]] + b[c])
\Gamma[u] = \sigmoid(W[u][C[t-1], x[t]] + b[u]) // Most of the time, it'd be either zero or one
\Gamma[r] = \sigmaid(W[r][C[t-1], x[t]] + b[u])
C[T] = \Gamma[u] * C'[t] + (1-\Gamma[u]) * C[t-1]
```

LSTM works similarly. It was written by Hocheriter & Schmidhuber 1997.

It works like this:

```
C'[t] = tanh(W[c][a[t-1], x[t]] + b[c])
\Gamma[u] = \sigmoid(W[u][a[t-1], x[t]] + b[u]) // Update
\Gamma[f] = \sigmoid(W[f][a[t-1], x[t]] + b[f]) // Forget
\Gamma[o] = \sigmoid(W[o][a[t-a], x[t]] + b[o]) // Output
C[t] = \Gamma[u] * C'[t] + \Gamma[f] * C[t-1]
a[t] = \Gamma[o] * tanh(C[t])
```
