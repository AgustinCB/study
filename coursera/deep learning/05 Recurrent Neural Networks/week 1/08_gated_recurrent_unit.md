# Gated Recurrent Unit (GRU)

In this lesson we will learn how to use GRU, a modification of RNN that helps with the vanishing gradient problem.

We had:

```
a[t] = g(W[a][a[t-1], x[t]] + b[a])
```

GRU were designed in Cho et al., 2014 and Chung et al., 2014.

We will have a new variable called `C` that is a memory cell. It will be a vector of the values of C at a given time, were time is the index. For now, `C[t] = a[t]`.

```
C'[t] = tanh(W[c][C[t-1], x[t]] + b[c])
\Gamma[u] = \sigmoid(W[u][C[t-1], x[t]] + b[u]) // Most of the time, it'd be either zero or one
C[T] = \Gamma[u] * C'[t] + (1-\Gamma[u]) * C[t-1]
```

Because `\Gamma[u]` would be very close to zero when you choose to keep the old activation value, it helps with the vanishing problem.

This is a simplief version. Let's see now the full GRU unit:

```
C'[t] = tanh(W[c][\Gamma[r] * C[t-1], x[t]] + b[c])
\Gamma[u] = \sigmoid(W[u][C[t-1], x[t]] + b[u]) // Most of the time, it'd be either zero or one
\Gamma[r] = \sigmaid(W[r][C[t-1], x[t]] + b[u])
C[T] = \Gamma[u] * C'[t] + (1-\Gamma[u]) * C[t-1]
```

`\Gamma[r]` decides how important is the previous value of `C[t-1]`.
