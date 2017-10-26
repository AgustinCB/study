# Random initialization

Now we will talk about how to initialize the cluster centroids and how to avoid falling in local optimums.

You should always try to set `K` to be minor than the number of examples. We would usually randomly pick K training examples and set them as the initial K examples.

The algorithm may end up at different solutions depending on the initial solution. In particular, it's possible that it ends up at a local optimum of the function J.

To avoid this, we would usually initialize K-means multiple times and run it multiple times:

```
for i=1 to 1000 {
  Randomly initialize K-means
  Run K-means, get c and \mu.
  Compute cost function (distortion)
}
result = min(cs), min(\mus)
```

If you're running K-means with a low number of cluster, this would help you to avoid local optimums. With high number classes it's unlikely to help a lot, but it will make a difference with small numbers.
