# GloVe word vectors

In this lesson we will see the GloVe algorithm, created in Pennington et. al., 2014. It means "global vectors to word representation."

A globe algorithm starts of by defining `x[i][j]` as the number of times the word `i` appears in the context of `j`. We have that `x[i][j] = x[j][i]`

GloVe minimizes the function `sum(1, 10k, i, sum(1, 10k, j, f(x[i][j])(\Theta[T][i] * e[j] + b[i] + b'[j] - log(X[i][j])) ^ 2))`

Where `f(X[i][j])` is called weighting term and `f(x[i][j]) = 0 if x[i][j] = 0`. It can be a function that gives a meaningful amount of computation to non-frequent words and doesn't give too much weight to frequent words.

Because `\Theta[T][i]` and `e[j]` are simetrical, we can average them.
