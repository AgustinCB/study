# When to change dev/test sets and metrics

In this lesson we will see when to move your target.

Let's say that you built a classifier of cats. And you have:

Metric: Classification error.
Algorithm A: 3% error.
Algorithm B: 5% error.

However, algorithm A is letting in some pornographic images. So for your companies point of view, algorithm A is a much better alternative. So the metric and your dev set prefer algorithm A, but you and your users perfer algorithm B. So this is a sign that you should change your sets or your evaluation metric.

So we have: `1/m[dev] sum(1, m[dev], i, y[pred] != y[i])`. However, it treats pornographic and non-pornographic photos equally. We can instead use: `1/m[dev] sum(1, m[dev], i, w[i] * y[pred] != y[i])`. Where w[i] would be 1 if x[i] isn't porn and 10 if z[10] is porn.

So far we have only discussed how to define a metric to evaluate classifiers. Worry separately about how to do well on this metric. This is what orthogonalization is about.
