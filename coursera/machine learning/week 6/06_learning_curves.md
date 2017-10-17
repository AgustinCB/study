# Learning curves

In this lesson, we will cover learning curves. They're a useful thing to plot to see if your algorithm is performing as expected or to check if the performance can be improved. They are used to diagnose if a model suffers from bias/variance problems.

A learning curve is a plot of the validation and training error is with respect of the training set size. To do so, you'd artificially reduce the size of the training set and calculate the errors for that reduced dataset.

For the training error:

Usually, for very small set sizes, we will see very small training errors. Because it'd be very easy to fit perfectly to that sets.

As it grows, you'll find that the average training set error will increase.

For the cross validation error:

When small training sets, we won't generalize well, so the error will tend to be higher. At the same time, as the set size increases, the more we will be able to generalize and the lesser the error will be.

## Learning curves with high bias

For the validation error:

When you have a high bias (overfitting), as you increase the training set size, you will find that the error rarely changes. So at the beginning it will decrease a bit and after that it will almost become an straight line.

Something similar will happen to the training set, but starting with low error. In fact, they both will eventually almost meet in a relatively high error value.

If a learning algorithm is suffering from high bias, getting more training data will not (by itself) help much, as obvious when seeing the straight line of the error.

# Learning curves with high variance

For training error:

With small training sets and a very high degree polynomio (plus an small value of \lambda), we will end up with a very weird model that fits the data perfectly and therefore has a very low error rate. As the set grows in size, it will become harder to fit perfectly the data, so the error rate will increase, but still be pretty low.

For cross validation error:

In this case, the error will start very high and remain very hihg as the size increase. A typical indication of a variance problem is the big gap between the training error and the cross validation error. 

The intuition says that if we keep adding more training data, that will likely help to reduce said gap and maybe solve the problem.
