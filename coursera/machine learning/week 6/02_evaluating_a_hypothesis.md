# Evaluation a hypothesis

In this lesson, we will cover how to evaluate a hypothesis that we created.

When we chose the parameters for our learning algorithm, we try to minize the error with the existing data. One may think that getting a really low error rate may be a good thing, but we know that it can cause overfitting and not generalize new examples.

In problems with a large number of features it becomes impossible to plot how our hypothesis looks like, so we need a good way to measure its performance. The standard way is:

Suppose that we have 10 training examples with one feature and one output. We will split our dataset in two parts, one called training set and another one called test set. A typical split is 70%-30%. It's often good to use randomly sorted data before splitting the samples.

What you'd do is learn the parameter \theta from the training set (minimizing the training error). And then you compute the test set error:

For linear regression:

```
J[test](\theta) = 1/(2 * m[test]) * sum(1, m[test], i, (h(x[test][i]) - y[test][i])^2)
```

For logisting regression:

```
J[test](\theta) = 1/(2 * m[test]) * sum(1, m[test], i, y[test][i] * log(h[\theta](x[test][i])) + (1-y[test][i]) * log(h[\theta](x[test][i])))
```

Sometimes there's an alternative for logistic regression called misclassification error:

```
err(h[\theta](x), y) = 1 if h[\theta](x) >= 0.5 and y = 0 or h[\theta](x) < 0.5 and y = 1
                       0 otherwise

J[test](\theta) = 1/m[test] * sum(1, m[test], i, err(h[\theta](x[test][i]), y[i]))
```

We can adapt this idea to chose the regularization parameter or the best features to use when designing a learning algorithm.
