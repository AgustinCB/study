# Gradient descent

In this lesson we will see how to use gradient descent to train how to learn our parameters.

We have the following cost function:

J(w, b) = 1/m sum(1, m, i, l(y'[i], y[i])) = -1/m sum(1, m, i, (y[i] log(y'[i]) + (1-y[i]) log(1-y'[i])))

We want to find w,b that minimize J(w,b). The fact that J is convex is one of the reasons we use: It doesn't have local minimums and we can trust what gradient descent says,

Refer to the course on machine learning on details on how gradient descent works.
