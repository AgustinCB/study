# Advanced optimization

We'll apply some optimizations to run logistic regression much more quickly with gradient descent.

Our objective is to minimize `J(\theta)`. Given `\theta`, we have code that can compute `-J(\theta)` and `\delta / (\delta \thetaj) J(\theta)` for `j=0,1,...,n`. Given that, gradient descent does:

```
Repeat {
  \theta j := \theta j - \alpha \delta / (\delta \thetaj) J(\theta)
}
```

So we can think of it as we should plug code into the algorithm that calculates the derivative of the cost function and then gradient descent uses that to calculate the minimum of the function. We don't actually need code to compute the value of `J(\theta)`, but we may want it to monitor conversions. So we will think ourselves as providing code for both of them.

## Optimization algorithm

Gradient descent isn't the only algorithm we can use, we also have others like: Conjugate gradient, BFGS, L-BFGS, which are more sophisticated. They both need the same input (a way to compute the cost and the derivative).

Advantages:
- No need to manually pick \alpha. It has a clever inner loop that automatically picks \alpha and can even use a different learning rate for every other ratio.
- Often faster than gradient descent.
Disvantages:
- More complex. There's a difference on performance between a good and a bad implementation of this algorithms and you need to be an expert in numerical computing to make it worth.

Example:

```
\theta = [\theta1 \theta2]'
J(\theta) = (\theta1 - 5) ^ 2 + (\theta2 - 5) ^ 2
\delta / (\delta \theta1) J(\theta) = 2(\theta1 - 5)
\delta / (\delta \theta2) J(\theta) = 2(\theta2 - 5)
```

The value in which `j(\theta)` is minimal is `\theta = [5 5]'`.

To apply one of the advanced algorithm to calculate the minimum of this function is implementing a function to calculate a cost function receiving `\theta` and returning the value and a gradient, the second one being an array that returns the two partial derivatives of the function on that value.

With that, you can use the `Optim` library in Julia. Which accepts that function, the inital `\theta` and the type of algorithm to use.
