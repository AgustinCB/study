# Batch norm at test time

Batch norm process your data mini-batch a time, but at test time you may want to do so one sample at the time.

So at training you'd do:

```
\mu = 1/m sum(1, m, i, z[i])
\sigma2 = 1/m sum(1, m, i, (z[i] - \mu)^2)
znorm[i] = (z[i] - \mu) / sqrt(\sigma2 + \epsilon)
zprim[i] = \gamma znorm[i] = \beta
```

Because you can't calculate the average and variance of one value, you will estimate them using exponentially weighted average (across mini-batches). So for every mini-batch you'd have one value \mu and \sigma2, which will be our estimate.

So in test time you'd compute znorm and zprom using the values of \mu and \gamma with the latest value that you had in the exponentially weighted averages.
