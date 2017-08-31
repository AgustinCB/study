# Feature scaling.

Suppose we have a problem with two features:

```
x1 = size of the house (0-2000 feet ^ 2)
x2 = number of bedrooms (1-5)
```

If you plot the contour plot of this problem, you will notice that the countours will be very elliptical, with a very tall and skiny form. If you run gradient descent in this kind of function, they can take a long time to arrive to the local minimum.

In general, the algorithm works better with contours with circular form.

In this cases, it's useful to scale the features:

```
x1 = size (feet ^ 2) / 2000
x2 = number of bedrooms / 5
```

Because now all the values of `x1` and `x2` are between `0` and `1`, the contours have a wider and smaller form and therefore the algorithm performs faster.

In general, we want every feature to be in the range `-1 <= xi <=1`. The limits aren't particularly important as long as they in the order of the order of `-3` to `3` or `-1/3` to `1/3`. Wider values may need to be scaled.

## Mean normalization

We usually want the features to have a zero mean. To do so we usually replace `xi` with `xi - ui` (except for x0), where `ui` is close to the averege of the values in the feature.

Then the formula becomes:

```
x = (xi - ui) / si
```

Where `ui` is the average value of `xi` in the training set and `si` is the range of the training set (i.e. maximum value minus minimum value) or the standard deviation.
