# Diagnosing bias vs variance

When your learning algorithm doesn't work as you'd expect, almost always is because you have either a high bias problem or a high variance problem, in other words, either an overfitting or underfitting problem. It's very important to know which it's to know how to improve your algorithm. That's what we will cover in this lesson.

As mentioned before, a very simple hypothesis often causes underfit, while a very complex one that covers all the points in our training set often causes overfitting. Now that we have the concept of training, validation and test sets, we can understand the concept of bias and variance a little bit better.

Let's considere the training and cross validation error:

```
J[train](\theta) = 1/(2 * m) * sum(1, m, i, (h[\theta](x[i]) - y[i])^2)
J[cv](\theta) = 1/(2 * m[cv]) * sum(1, m[cv], i, (h[\theta](x[cv][i]) - y[cv][i])^2)
```

If you plot the degree of polynomial over the those errors, you'll note that in the case of the training error the plot will start with a high error on low degree polynomios and decreases as you increase the degree. As of the cross validation error (or even the test error), you'll a high cross validation error on very low degree polynomios, which will decrease to increase later.

So the first one will be similar to the function `1/x` for x > 0 and the second one will look like a parabole. 

If your learning algorithm is performing less well than you were hoping (the variance or test error is high), then we have a high bias problem if the degree is low and high variance otherwise if it's too high.

If the problem is bias, all your error variables will be high.

If your problem is variance, the training error will be low while the cross validation and/or the test sets will be much bigger.
