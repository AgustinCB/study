# Why regularization reduces overfitting

In this lesson we will gain intuition about how regularization works.

Let's say we have a neural network that is currently overfitting. When we regularize, we add a term that penalizes the function for being too large. By cranking lambda to be really big, we will reduce the impact of a lot of hidden units still having something similar to a less complex network. That will reduce the variance until being roughly linear.
