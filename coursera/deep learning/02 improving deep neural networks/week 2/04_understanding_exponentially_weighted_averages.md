# Understanding exponentially weighted averages

In this lesson we will speak about how to use exponentially weighted averages in algorithms to optimize functions.

We have `v[i] = \beta v[i-1] + (1-\beta) \theta[i]`. In this case we can say that: `v[i] = (1-\beta) sum(i, 1, j, \theta * \beta^(j-1))`. The number of days that it keeps in mind when computing v, is the power of it that equals 1/e. This is a rule of thumbs and not an acutal mathematical fact.

Usually, you want to keep only the latest value, so it needs very small memory to be implemented.
