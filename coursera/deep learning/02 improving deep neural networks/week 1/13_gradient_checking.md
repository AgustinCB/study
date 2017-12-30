# Gradient checking

Gradient checking is a technique to find bugs in an implementation of backpropagation.

First take all to your parameters and reshape them into a big vector \theta. So we will have a function `J(\theta)` instead of our original J. You should also reshape all the derivatives into a big dtheta vector.

So the question is now: Is dtheta the gradient of the cost function J?

You will do:

```
for each i in size(\theta):
  dthetaapprox[i] = (J(..., \theta[i] + \epsilon, ...) - J(..., \theta[i] - \epsilon, ...))

assert(distance(dtheta, dthetaapprox) < 1e-6)
```

Where `distance(v1, v2) = (||v2 - v1||) / (||v2||[2] + ||v1||[2])`.

If the assert fails, there's probably a bug in the implementation of backpropagation.
