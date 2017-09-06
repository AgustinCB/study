# Cost function

In this lesson we will talk about how to define a cost function to chose the parameters for our hypothesis.

Suppose a training set: `{(x1, y1), (x2, y2), ..., (xm, ym)}` with m examples and `x` in `[x0 x1 ... xn]'`, x0 = 1, y being `{0,1}` and `h(x) = 1 / (1 + e^(\theta' x))`. How do we choose parameters \theta?

In linear regression our cost function was: `J(\theta) = 1/2m (sum(1, m, (h(xi) - yi) ^ 2)) = 1/m sum(1, m, cost(h(xi), y))` where `cost(h(x), y) = 1/2 (h(x) - y) ^ 2`. This works fine for linear regression, but because is non-convex on logistic regression (with many local optims) it is not guaranteed that our gradient descent would converge on the global minimum. We would like a different cost function that is convex.

We will use:

`cost(h(x), y) = -log(h(x)) if y = 1 else -log(1-h(x))`

If we plot that for y = 1, we get a function scoped between x in `[0, 1]` with (1,0) and y tending to inf as x approaches zero. Which captures the intuition that if h(x) = 0 (predict P(y=1|x;\theta) = 0) but y = 1, we have to penalize the learning algorithm by a very large cost.

If `y=0`, `cost = -log(1 - h(x))`, which plotted looks like the opposite of the previous one: It passes through (0,0) and goes to inf as it approaches 1 (with the same benefits).
