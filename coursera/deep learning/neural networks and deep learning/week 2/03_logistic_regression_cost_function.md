# Logistic regression cost function

In this lesson we will take a look at a cost function we can use in logistic regresion.

We had `y' = \epsilon(w'x + b)` where `\sigma(z) = 1 / (1+e ^ -z)` and `z[i] = w'x[i] + b`.

So we want w so that given (x[1], y[1]), ..., (x[m], y[m]), y'[i] is close to y[i].

We can use the following error function: l(y', y) = 1/2 (y' - y)^2. But that has a non-convex optimization problem and gradient descent may not perform as well.

But the idea is good: We need a function (l) that defines the loss between our prediction and the actual output.

What we do use in logistic regression is: l(y', y) = -(y log(y') + (1-y) log(1-y'))

This makes sense because when y = 1, we only apply the first term. That means that we want log(y') to be as big as possible and that means to have y' to big as possible. When y=0, on the other hand, we only apply the second term. And then we want log(1-y') to be big. And that means that y' has to be small. All this with y' between zero and one.

There're other functions with similar effect, but there're more reason to choose this one in particular.

This works for a single example, but we want also a function to know how we are doing in the entire training set. A cost function.

J(w, b) = 1/m sum(1, m, i, l(y'[i], y[i])) = -1/m sum(1, m, i, (y[i] log(y'[i]) + (1-y[i]) log(1-y'[i])))
