# Logistic regression with gradient descent

We will see how to use the computation graph for deriving gradient descent for logistic regression.

We have:

z = w'x + b
y' = a = sigmoid(z)
l(a, y) = -(y log(a) + (1-y) log(1-a))

Let's say we have only two features. Then:

z = w[1] x[1] + w[2] x[2] + b

In going backwards, we want to compute the derivative of l(a, y) with respect to a. This ends up being -y/a + (1-y)/(1-a). The derivative of l(a, y) with respect to z would be in a - y. The last step would be to calculate for w[1] (`x[1] dz`), w[2] (`x[2] * dz`) and b (dz).
