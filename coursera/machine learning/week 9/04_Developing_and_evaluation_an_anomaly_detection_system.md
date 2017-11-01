# Developing and evaluating an anomaly detection system

In this lesson we will cover the process of developing specific applications of the problem and how to evaluate.

## The importance of real-number evaluation

When developing a learning algorithm (choosing features, for example), making decisions is much easier if we have a way of evaluating our learning algorithm.

We will need a way to evaluate our anomaly detection system. Assume we have some labeled data, of anomalous and non-anomalous examples (y=0 if normal, y=1 if anomalous).

Our training set will be assumed to be normal, not anomalous examples.

Then we will define our cross validation set and our test set to have some anomalous examples.

For example, supposing we have 10000 good/normal examples and 20 flawed ones, a fairly typical way would be to put 6000 good examples in the training set and 2000 good and 10 anomalous for the cross validation and test sets.

## Algorithm evaluation

Fit model p(x) on our training set.

On a cross validation/test example x, predict

```
y = 1 if p(x) < \epsilon
    0 if p(x) >= \epsilon
```

Because our data is very skewed (y=0 is much more common), our possible evaluation metrics are:

- True positive, false positive, false negative, true negative.
- Precision/Recall
- F[1]-score

We can also use cross validation set to choose the parameter \epsilon, similar to what we can do with \alpha in regression algorithms. 
