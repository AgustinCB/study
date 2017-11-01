# Anomaly detection vs supervised learning

In this lesson we will cover when to use anomaly detection versus supervised learning.

When to choose anomaly detection:

- When you have a problem with a very small number positive examples (y=1) (0-20 is common),
- When there are many different types of anomalies. It can be hard for any algorithm to learn from an small set of positive examples what the anomalies look like. Also future anomalies may look nothing like any of the anomalous examples we've seen so far.

When to choose supervised learning:

- When you have a large number of positive and negative examples.
- When you have enough positive examples for an algorithm to get a sense of what they look like and you think that future positive examples are likely to be similar to the ones in the training set.
