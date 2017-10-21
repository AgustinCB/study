# Kernels II

In the last lesson we introduced the idea of Kernels. Now we are going to see how to use this ideas in practice.

## Choosing the landmarks

In the last lesson, we picked the landmarks by hand so we can use the kernel function to create new features.

How are we going to do this when we want a bunch of landmarks that are unpractical to get by hand?

This is the idea: We are going to get our training set, and we are going to set our landmarks to the exact same points in the training set. So the feature vector of the example x, will be the vector with the similarity of that sample with all the ones in the training set. One of this features will be exactly one, because we're going to check the similarity between two exact points. Now we can represent our training sample as a feature vector with this new values.

Given all this, we have the following hypothesis:

Given x, compute the features f in R^(m + 1) and predict `y=1` if `\theta'f >= 0`.

Now we have the minimization problem:

```
J[\theta](x) = C A[\theta](f) + B(\theta)
A[\theta](x) = sum(1, m, i, y[i] * cost[1](\theta'f[i]) + (1-y[i]) cost[0](\theta'f[i]))
B(\theta) = sum(1, n, i, \theta[i] ^ 2) / 2 = \theta'\theta (or \theta'M\theta)
```

As a note, we will have the same number of features as of samples, so `n=m` in this particular case.

You can also apply this idea to algorithms like logistic regression, but that will be computationally slow.

## How to chose parameters

To choose the parameter C (similar to 1/\lambda), you have to keep in mind that a large value of C we will have lower bias and high variance. While an small value will cause higher bias and low variance.

To choose the parameter \sigma^2, we have to keep in mind that a large value will cause the features f[i] to vary more smoothly. It also causes higher bias and lower variance.

An small value will cause the features to vary less smoothly and will give the hypothesis lower bias and higher variance.
