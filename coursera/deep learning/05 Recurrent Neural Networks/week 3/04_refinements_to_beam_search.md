# Refinements to beam search

In this lesson we will how to change beam search to make it work even better.

We said that beam search maximizes this function:

```
f(x,y) = prod(1, Ty, t, P(y[t] | x, y[1], ..., y[t-1]))
```

We start with length normalization.

Multiplying numbers that are a lot less than one, will result in very small numbers that will cause problems to the computer because of the details on how floating point numbers are represented. To prevent rounding errors, we can use `log`:

```
f(x,y) = sum(1, Ty, t, log(P(y[t] | x, y[1], ..., y[t-1])))
```

It should give the same value and it will simplify things.

Another problem is that it tends to prefer short translations because it's either multiplying small numbers (first case) or summing small negative numbers. We can prevent this by normalizing the result:

```
f(x,y) = 1/Ty^-\alpha sum(1, Ty, t, log(P(y[t] | x, y[1], ..., y[t-1])))
```

We use \alpha to control how much normalization we want to use. We will use this score to evaluate all the sentences found by beam search and select the highest selection.

The larger `B`, the better results but the more computing you need. In production systems, you often see `B` of 10 and in research about 1000-3000. As `B` increases, though, it gets diminishing returns. Beam search runs fast but is not guaranteed to find exact maximum results.
