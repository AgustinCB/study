# The problem of local optima

In the early days of machine learning, it was often a fear that the algorithm would get stuck in a local optimum instead of find its way to a go to a global optimum.

In cases where there's a lot of features (high dimensional spaces), is more likely that a zero gradient correspond to a saddle point rather than a local optimum.

A more valid concern is a plateaus: Places where the derivative is close to zero for a long time. That makes gradient descent go very slowly.
