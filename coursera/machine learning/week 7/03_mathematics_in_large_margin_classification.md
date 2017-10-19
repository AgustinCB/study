# The mathematics behind large margin classification

In this lesson we will cover the mathematics behind large margin classification and the support vector machine.

## Vector inner product

Let's say we have two vectors in R^2, u and v. Then we say that the inner product of them is u'v. The norm of a vector is defined as `||u|| = sqrt(u[1] ^ 2 _ u[2] ^ 2)`.

To compute the inner product of u and v, we take v and project it onto the vector u. We can then the lenght of that vector and call it p. Then, we can say that `u'v = p * ||u|| = u[i] v[i] + u[2] v[2]`.

The length p is a signed real (if the angle between u and v is less than 90 degrees, then p is positive and negative otherwise).

## Optimization objective

```
min[\theta](1/2 sum(1, n, j, \theta[j]^2))
s.t. \theta'x[i] >= 1 if y[i] = 1
     \theta'x[i] <= -1 if y[i] = 0
```

we will assume there is no bias unit.

When n=2, that can be writen as `1/2 (\theta[1]^2 + \theta[2]^2)` or `1/2 sqrt(\theta[1]^2 + \theta[2]^2) = 1/2 ||\theta|| ^ 2`. So we're minimizing the square of the norm of the vector \theta.

So we want to see what \theta'x[i] looks like for a given input x[i]. As discussed, `\theta'x[i] = p[i] * ||\theta|| = \theta[1] * x[i][1] + \theta[2] * x[i][2]`.

So we can say that our optimization objective is now:

```
min[\theta](1/2 ||\theta||^2)
s.t. p[i] * ||\theta|| >= 1 if y[i] = 1
     p[i] * ||\theta|| <= -1 if y[i] = 0
```

Our decision boundary will be orthogonal to the parameters \theta chosen by this minimization problem. Because of that, the training examples that are close to the decision boundary will have a very small value of p. If that's the case, for our algorithm to satisfy our conditions, we will need ||\theta|| to be pretty large. Because we're trying to minimize the norm of the \theta, that's very unlikely to happen! That's why the decision boundary will be in the place where the margin with the samples is bigger (because by making the margin large, p is large and ||\theta|| is small).

Adding a bias unit has the same effect.
