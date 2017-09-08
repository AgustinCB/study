# Simplified cost function and gradient descent

We will try to figure out an slightly simpler cost function and how to implment gradient descent over it.

The function so far:

```
J(\theta) = 1/m sum(m, 1, cost(h(xi), yi))
cost(h(x), y) = -log(h(x)) if y = 1 else if y = 0 -log(1-h(x))
```

Note that y = 0 or 1 always. We will see how to write the cost function in one statement:

```
cost(h(x), y) = -y log(h(x)) - (1-y) log(1-h(x))
```

To probe that they are equal:

- Let's suppose that y = 1, then:

```
cost(h(x), y) = -y log(h(x)) - (1-y) log(1-h(x)) = -y log(h(x)) - 0 log(1-h(x)) = -log(h(x))
```

- On the other hand, if y = 0, then:

```
cost(h(x), y) = -y log(h(x)) - (1-y) log(1-h(x)) = 0 log(h(x)) - log(1-h(x)) = -log(1-h(x))
```

Then, we have that:

```
J(\theta) = 1/m sum(1, m, cost(h(x), y)) = -1/m sum(1, m, yi * log(h(xi)) + (1-yi) * log(1-h(xi)))
```

Why to choose this while there could have been other functions? This cost function could be derived from statistics using the principle of likelihood estimation, which is an idea for how to effitienly find parameters for models and it also has the nice property that is convex.

Then our objective will be to minimize `J(\theta)` and use that get our prediction using the model and the output will be `p(y=1|x;\theta)`.

So, how do we minimize it?

## Gradient descent

Again, our gradient descent algorithm is:

```
Repeat {
  \thetaj := \thetaj - \alpha \delta / (\delta \theta j) J(\theta)
}
```

Which in this case:

```
\delta / \delta \thetaj J(\theta) = 1/m sum(1,m, (h(xi) - yi) xji)
```

And so:

```
Repeat {
  \thetaj := \thetaj - \alpha \delta / (\delta \theta j) J(\theta) = \thetaj - \alpha/m sum(1,m, (h(xi) - yi) xji)
}
```

Algorithm looks identical to linear regression! The only thing that changed is the definition of `h(x)`.
