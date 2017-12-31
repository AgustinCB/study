# Learning rate decay

Something that may help speed up gradient descent is slowly decreasing alpha over time. This is called learning rate decay.

If you're using mini-batch gradient descent, you won't converge in the minimum. However, if you make alpha slowly decrease, it will end up gravitating very closely to the minimum.

You can set alpha as `alpha = 1 / (1+ decay_rate * epoch_num) \alpha0`. Here you have now two hyper parameters: \alpha0 (the first alpha) and `decay_rate` (how fast alpha goes down). Another options:

```
\alpha = 0.95 ^ epoch_num \alpha0
\alpha = k / sqrt(epoch_num) \alpha0
\alpha = \alpha / (t//K)
```

Learning rate decay usually doesn't help as much as other techniques.
