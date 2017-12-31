# Gradient decent with momentum

In this less we will see how to compute gradient decent with momentum by usingexponentially weighted averages.

In cases in which is common to have oscilation in gradient descent, you won't be able to use large learning rates. To prevent this, you can use momentum:

On iteration t, you will compute the usual derivatives: dw, db on the current iteration. Then you compute:

```
Vdw = \beta Vdw + (1-\beta) dw # (it's doing a moving average)
Vdb = \beta Vdb + (1-\beta) db
W = W - \alpha Vdw
b = b - \alpha Vdb
```

This will smooth out gradient descent. You will initialize Vdw and Vdb to zero.
