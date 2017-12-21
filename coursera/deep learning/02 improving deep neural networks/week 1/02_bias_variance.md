# Bias/Variance

In this lesson we will the bias/variance trade off on deep learning.

Suppose that we have three models, one that has high bias, one that works just fine and one that has high variance.

To try to understand bias and variance, there are a couple metrics to look at:

- Train set error.
- Dev set error.

Supposing that the optimal (Bayes) error, is close to zero.

Let's say that we have a training set error of 1% and 11% on dev set. That would be a high variance case.

If we had 15% and 16% error, then we would have high bias, because it's not even fitting the training set.

Let's say that we have 15% error on the training set and 30% dev set error. That would have high high bias and high variance.

On the case of 0.5% and 1%, it'd be low bias and low variance.
