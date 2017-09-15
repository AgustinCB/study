# Regularization

In this lesson we will cover how regularization works and what new cost function we will use for it.

## Intuition

Suppose that we have the problem of predicting the price of a house based on its size. And we have a model `\theta0 + \theta1 x + \theta2 x^2 + \theta3 x^3 + \theta4 x^4` that overfits. And then we decide to penalize and make \theta3 and \theta4 really small. For example:

```
min(\theta, 1/2m sum(1,m,(h(x) - y) ^ 2) + 1000 \theta2 ^ 2 + 1000 \theta4 ^ 2)
```

When we try to minimize this, we will end up with \theta3 and \theta4 close to zero and a close to quadratic function.

In this example, we tried to penalize two of the parameters.

## Regularization

The general idea of regularization is to have very small values for parameters. This will get a simpler hypothesis which are less prone to overfitting.

In the example of the housing problem, we may have a hundred features. In this context is difficult to pick which feature is more relevant and therefore is difficult to pick which of the 101 parameters that we have to regularize.

So we will modify the cost function to shrink all of our parameters:

```
J(\theta) = 1/2m sum(1,m, (h(x) - y) ^2) + \lambda sum(1, m, \theta ^ 2)
```

As you can see, we aren't including \theta0 in the regularization, thi is by convention. The term in the right is the regularization term and \lambda hwere is the regularization parameter, which controls the tradeoff of fiting the data well and at the same time keeping the parameters small and the hypothesis simple.
