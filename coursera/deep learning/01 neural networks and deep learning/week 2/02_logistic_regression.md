# Logistic regression

In this lesson we will go over logistic regression, an algorithm for binary classification problems.

Given the vector X, we want an algorithm that can output `y' = P(y=1|x)`, i.e., the probability of `y` being 1 for that input x. The input will be a n-dimensional vector. The parameters of logistic regression will be w, also a n-dimensional vector, and b, which is a real number. With that, we can have `y'` be:

```
y' = sigmoid(w'x + b)
sigmoid(z) = 1 / (1 + e^(-z))
```

If z is large, g(z) will be close to one. If z is very small, g(z) will be close to zero.

We want to find w so that all our x produce a probability close to the actual output.

In some cases, you'd define an extra feature `x[0] = 1` and an extra parameter in then you can merge w and b in one big array. We will not be using this notation in this course.
