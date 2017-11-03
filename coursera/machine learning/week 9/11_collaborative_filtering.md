# Collaborative filtering

In this lesson we will cover how to build a recommendation system called collaborative filter. It learns by itself what features to use.

Let's suppose again the example of the movie ratings and suppose that we don't know how to categorise them.

Let's also say that we want through all the users and they told how much they like each category and we represent that with (biased) vector \theta.

Let's suppose that we have an example of a movie rated well by users who like romantic movies but it's hated by users who dislike them. Then we can tell that it's a romantic movie. Using this we can categorise each movie.

# Optimization algorithm

Given \theta[1:n], we want to learn x[i]:

```
min(x[i]) 1/2 sum(j:r(i, j)=1, (\theta[j]'x[i] - y(i, j))^2 + \lambda/2 sum(1, n, k, x[i][k]^2))
```

This is how we will learn the features for one specific movie.

```
min(x[1:n[m]]) 1/2 sum(1, n[m], i, sum(j:r(i, j)=1, (\theta[j]'x[i] - y(i, j))^2)) + sum, 1, n[m], i, \lambda/2 sum(1, n, k, x[i][k]^2))
```

This is how we will learn the feeatures for all the movies.

So we have that given x[1:n[m]] we can estimate \theta[1:n[u]]. And that given \theta[1:n[u]], we can estimate x[1], ..., x[n[m]].

What we can do is to estimate our initial \theta[1:n[u]], then calculate x, calculate \theta again and so on still you have something that seems stable.
