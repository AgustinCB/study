# Principal component analysis: Problem formulation

For the problem of dimensionality reduction, the most common algorithm is Principal Component Analysis (PCA). Now we will cover the formulation of that algorithm.

## Problem formulation

Let's say we have a datasets of two inputs and we want to reduce it to one dimension. In other words, we want to find a line to project the data into. Let's also assume that we denormalized and scaled the data.

We need a line whose distance with every point is very small. We can then define the distance of a point to the line as the project error and say that we want to minimize the sum of squares of those values.

We then can say that we want to find k direction vectors (u[i] in R^n) onto which to project the data so as to minimize the projection error.

It's similar in concept to linear regression, but actually a very different algorithm.
