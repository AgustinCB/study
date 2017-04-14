# Linear Regression

We will now apply the gradient descent algorithm into the model representation we did before to calculate the local minimums. First, we need to understand how the derivative behaves:

```
d/(d*CJ) * J(C1, C2) =
d/(d*CJ) * 1/(2*m) * sum(1, m, i, (h(xi) - yi)^2) =
d/(d*CJ) * 1/(2*m) * sum(1, m, i, (C1 + C2 * xi - yi)^2)
```

We want to see the value for C1 and C2:

```
C1 => d/(d*C1) * J(C1, C2) = 1/m * sum(1, m, i, h(xi) - y1)
C2 => d/(d*C2) * J(C1, C2) = 1/m * sum(1, m, i, (h(xi) - y1) * xi)
```

## Add Gradient descent algorith

Now we have our linear regression algorithm:

Repeat until convergence:

```
C1 := C1 - a * d/(d * C1) * J(C1, C2) = C1 - a * (1/m * sum(1, m, i, h(xi) - y1))
C2 := C2 - a * d/(d * C2) * J(C1, C2) = C2 - a * (1/m * sum(1, m, i, (h(xi) - y1) * xi))
```

Because this function is a convex function and they only have one local optimum (the global one), gradient descent will always converge to that value.

This is sometimes called "batch" gradient descent. Batch here means that each step of gradient descent uses all the training examples. There are some version of gradient descent that don't use all the training examples.
