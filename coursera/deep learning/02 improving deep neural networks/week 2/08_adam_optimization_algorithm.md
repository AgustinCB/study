# Adam optimization algorithm

In this lesson, we will see how to use previous knowledge (RMS and momentum) to improve gradient descent using something called Adam optimization algorithm.

You'll do:

```
Vdw = Sdw = Vdb = Sdb = 0
On iteration i:
  compute dW and db using current mini-batch
  Vdw = \beta[1] Vdw + (1 - \beta[1]) dW
  Vdb = \beta[1] Vdb + (1 - \beta[1]) db
  Sdw = \beta[2] Sdw + (1 - \beta[2]) dW
  Sdb = \beta[2] Sdb + (1 - \beta[2]) db
  Vdwcorrec = Vdw / (1-\beta[1]^i)
  Vdbcorrec = Vdb / (1-\beta[1]^i)
  Sdwcorrec = Sdw / (1-\beta[2]^i)
  Sdbcorrec = Sdb / (1-\beta[2]^i)
  W = W - \alpha Vdwcorrec/(sqrt(Sdwcorrec) + \epsilon)
  d = b - \alpha Vdbcorrec/(sqrt(Sdbcorrec) + \epsilon)
```

We have now this hyperparameters:

- \alpha: Needs to be tuned.
- \beta[1]: Usually 0.9
- \beta[2]: Usually 0.999
- \epsilon: Usually 1e-10 (doesn't affect performance much at all and usually doesn't matter).

Adam stands for Adapted moment estimation.
