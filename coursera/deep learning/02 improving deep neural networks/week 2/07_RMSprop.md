# RMSprop

In this lesson, we will see how to use RMSprop (root mean square prop) as an alternative for classic gradient descent.

We want to reduce the oscilations of gradient descent. To do so, on iteration t, we will compute dW and db as usual in the current mini-batch and then:

```
Sdw = \beta Sdw + (1-\beta) dW^2
Sdb = \beta Sdb + (1-\beta) db^2
W = W - \alpha dW/sqrt(Sdw)
b = b - \alpha db/sqrt(Sdb)
```

In the directions in which the slope is very large (the ones that causes the oscilations), the updates will be divided by a much larger number and therefore it will keep the divergions under control.

There are cases in which you use both momentum and RMSprop. In those cases, you have two different \beta hyperparameters and not only one.
