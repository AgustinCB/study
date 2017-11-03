# Vectorization: Low rank matrix factorization

In this lesson we will cover the vectorization implementation of the algorithm and another things that we can do with it.

## Alternative way to write it

Suppose that we have a matrix of size n[m]xn[u] with the rating give to movie i by user j in the element x[i][j]. Because of this that value is defined by \theta[j]'x[i]. We call it Y.

Then we define the matrix X, which is composed by column vectors of each movie. And then we define the matrix \Theta, with column vectors composed by the different \theta.

So we can say: Y = X \Theta'. This is called low raw matrix factorization.

## Finding related movies.

For each movie i, we learn a feature vector x[i] in R^n with the important aspects of the movie.

How do you find movies j related to movie i?

We want to find a movie j such that ||x[i] - x[j]|| is small, which also means that they're similar.
