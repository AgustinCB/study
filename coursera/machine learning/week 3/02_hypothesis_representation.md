# Hypothesis representation

In this lecture we will cover what is the function that we will use to represent our data when we have a classification problem.

We said that we need an hypothesis whose output is in the range [0,1].

Previously, we had:

```
h(x) = \theta' * x
```

We will now use a function g on top of that:

```
h(x) = g(\theta' * x)
g(z) = 1 / (1 + e ^ z)
```

g is also called logistic function (or sigmoid function). This function is in the range desired. That give us:

```
h(z) = 1 / (1 + e ^ (\theta' * x))
```

When x is near -inf, then it the logistic function approaches 0. When it's 0, it's 0.5. And as it approaches +inf, it approaches 1.

## Interpretation of this model

```
h(x) = estimated probability that y = 1 on input x
```

For example, in the tumor classification example, suppose that we feed a new value into our hypothesis and our algorithm gives us 0.7. That means that the tumor has a 70% chance of being malignant.

This is also writen as:

```
h(x) = p(y=1|x;\theta) -- probability that y=1, give x, parameterized by \theta
```

Since this is a classification task, we know that y is 0 or 1. In this context, the probability of y=0, would be 0.3.
