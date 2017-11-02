# Content based recommendations

In this lesson we will cover a first approach to build our recommendation system.

How do we predict the missing ratings in the movie example of before?

We can try to represent each movie as feature vector with this form:

```
x[i] = [ 1 z[1] ... z[n] ]
```

Where z[i] represents the how much there's in the movie of a movie category.

Then for each user j, we can learn a parameter \theta[j] and we can use linear regression to predict the rating of a movie i with `\theta[j]'x[i]`.

In this case we use the category of a movie and the likelyhood that a user like it to predict it using our good old linear regression.

## Problem formulation

Let's say that m[j] is the number of movies rated by user j. Then to learn \theta[j] we want to:

```
min(\theta[j]) 1/(2m[j]) sum(i:r(i, j)=1, (\theta[j]'x[i] - y(i, j)) ^ 2) + \lambda / (2m[j\]) sum(1, n, k, \theta[j][k]^2)
```

So we want to learn \theta for all the users, so we want to:

```
min(\theta[1:n[u]]) 1/(2m[j]) sum(1, n[u], j, sum(i:r(i, j)=1, (\theta[j]'x[i] - y(i, j)) ^ 2)) + sum(1, n[u], j, \lambda / (2m[j\]) sum(1, n, k, \theta[j][k]^2))
```

After that we will use gradient descent as usual.

This is called content based recommendation, because it assumes that we have features of the movies (their categories).
