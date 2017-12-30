# Numerical approximation of gradients

In this lesson we will cover an introduction to a test gradient checking to make sure that the implementation of backpropagation is correct.

Let's say that we have `f(\theta) = \theta^3`. When \theta=1, the result is one. Let's say that we have \epsilon = 0.01, with that:

```
(f(\theta + \epsilon) - f(\theta - \epsilon)) / 2\epsilon = 3.0001
```

Which is close to `g(\theta) = f'(\theta)`: `g(\theta) = 3\theta^2 = 3`.

This is called "two-sided difference"

This is because:

```
f'(\theta) = lim(\epsilon, 0, (f(\theta+\epsilon) - f(\theta-\epsilon))/(2\epsilon))
```
