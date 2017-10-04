# Gradient checking

In the last lessons we covered backwards and forwards propagation.

Backwards propation has the unfortunate property that it's easy to make a hard to see mistake, because it seems to be performing correctly. To prevent this, we use an idea called gradient checking to prevent this.

Suppose that we have a function `J(\Theta)` and we want to approximate the derivative in one point, `\Theta`. What we would do is grab a very small `\epsilon` and grab the points `\Theta - \epsilon` and `\Theta + epsilon`, calculate their value in `J` there and calculate the slope of the line created by them, which is:

```
(J(\Theta+\epsilon) - J(\Theta-\epsilon)) / (2 \epsilon)
```

Which will be an estimated derivative of `J(\Theta)`, being more accurate as `\epsilon` approaches zero. THis is for `\Theta` being a real number.

When it is a vector, we will use a similar idea by applying this trick in the partial derivative of `J` on each one of the values of the vector `\Theta`.

```
d/(d\thetan) J(\theta) approx (J(\theta0, ..., \thetan+\epsilon, ...) - J(\theta0, ..., \thetan-\epsilon, ...)) / (2 \epsilon)
```

The way to use this is that our approximation is very close to the result of the backpropagation.

## Implementation

1. Implement backprop to compute `DVec`
2. Implement numerical gradient check to compute `gradApprox`.
3. Make sure they give similar values.
4. Turn off gradient checking. Using backprop code for learning. Since it's very slow and expensive.
