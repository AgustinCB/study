# Large margin intuition

Support vector machines are often refered as large margin classifiers. We will see now what that means and this will give us an useful picture of what a SVM hypothesis would look like.

Our cost function:

```
J[\theta](x) = C A[\theta](x) + B(\theta)
A[\theta](x) = sum(1, m, i, y[i] * cost[1](\theta'x[i]) + (1-y[i]) cost[0](\theta'x[i]))
B(\theta) = sum(1, n, i, \theta[i] ^ 2) / 2
```

To make it small (our objective), if `y=1`, it will be zero only if `\theta'x >= 1`.
At the same time, if `y=0`, it will be zero only if `\theta'x <= -1`.

This is interesting because our previous function only required `\theta'x` to be greater (or smaller) than zero and not one. SVM adds an extra safety margin by asking for more than one (or minus one).

Consider the case in which C is a very large case. In this case, we will be highly motivated to chose a value that will cancel C by converting the sum into zero. What will it take to get that?

Whenever y[i] = 1, we need to find a \theta so \theta'x[i] >= 1 (and the opposite to y[i] = 0).

In those cases, we will have to minimize the regularization term alone.

When we do the math, we get a very interesting decision boundary: It will be linearly separable. That line will have a large minimum distance from any of the training examples. This is called the margin of the support vector machine and gives robustness (and the name large margin classifier).

## Outliers

If C is very large, though, our decision boundery will be very sensitive to outliers and will change very quickly to try to fit samples that aren't in the same group as the rest. However, using a not too large C, that won't happen.
