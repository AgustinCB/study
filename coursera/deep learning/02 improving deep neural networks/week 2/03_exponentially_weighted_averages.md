# Exponentially weighted averages

In this lesson, we will see a few optimization algorithms that are faster than gradient descent and rely on exponentially weighted averages.

Suppose that we have a dataset with the wheather in London over the days of a year. Plotted, you will see an hyperbole with the pike in the middle of the year (around day 180) and the minimums are in the extrems.

We will compute now the local average:

```
v[0] = 0
v[i] = \beta v[i-1] + (1-\beta) \theta[i]
```

Where \theta[i] is the temperature for that day. If we plot that, we will see the local average for that particular day.

You can think of v[i] as approximate the average over 1/1-\beta days. As \beta increases, the curve of the average is smoother, as it decreases it adapts more to new changes, because it gives less importance to previous values.

In this case, if we use \beta=0.9, we will be averaging over 10 days. With 0.98, over 50 days. And with 0.5 over 2 days.
