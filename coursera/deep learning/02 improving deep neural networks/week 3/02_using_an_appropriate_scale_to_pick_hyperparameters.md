# Using an appropriate scale to pick hyperparameters

In this lesson we will see how to choose a random to sample when exploring hyperparameters.

Let's say that we are trying to choose then number of hidden units for a neural network and we think that the number is between 50 and 100. Or maybe the total number of layers and you think that the range would be between 2 and 4. You can choose randomly in there, but doesn't nicely extend to all parameters.

If you are trying to pick the learning rate and suspect that it would be between 0.0001 and 1 and you pick randomly, you will be picking 90% in values from 0.1 to 1.

Instead, we can use the logarithmic scale, and then spend more resources in small values.

```
r = -4 * np.random.rand() # log[10](0.0001) = -4
alpha = 10 ** r
```

Another weird case is sampling the beta value for exponentially weighted averages and you suspect it would be between 0.9 (averaging over the last 10 values) and 0.999 (averaging over the last 1000). Like before, random sampling won't give nice as you expend much more resources in the range [0.9,0.99]. Another way to think of this is sampling from `(1-\beta)` in the range from 0.1 and 000.1 and use the logarithmic trick again.

The reason to do this, is that a change in certain regimes are very sensitives to small changes in the hyperparameter than in other regimes.
