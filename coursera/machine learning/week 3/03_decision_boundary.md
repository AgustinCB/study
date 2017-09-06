# Decision Boundary

We'll try to understand now when our prediction does prediction that means y=0 and when those mean y=1.

Suppose predict y=1 if h(x)>=0.5 and predict y=0 if h(x) < 0.5. Let's try to understand when that happens.

You'll notice that the sigmoid function is greater or equals than 0.5 if z is bigger or equals to 0. Therefore the prediction will be one when `\theta' * x >= 0`. By a similar argument, the prediction will be 0 if `\theta' * x < 0`.

Suppose that we have `h(x) = g(\theta0 + \theta1 x1 + \theta2 x2)`. Suppose that `\theta = [-3 1 1]'`.

Then y = 1 if `-3 + x1 + x2 >= 0 => x1 + x2 >= 3`. Which will show a figure of a diagonal line that goes through (3,0) and (0,3). The part that is to the right of that line will be y = 1 and the part of the left will be y = 0. That line, `x1 + x2 = 3`, is the decision boundary and the place in which `h(x) = 0.5`. That decision boundary includes the parameters (\theta).

## Non-linear decision boundary

Suppose that our training set has an hypothesis like `h(x) = g(\theta0 + \theta1 x1 + \theta2 x2 + \theta3 x1^2 + \theta4 x2^2)` and the values of the parameters that we chose are `\theta = [-1 0 0 1 1]'`.

We will predict y=1 if `-1 + x1 ^ 2 + x2 ^ 2 >= 0 => x1 ^ 2 + x2 ^ x >= 1`. If we draw that function, we will see that the desicion boundery has the shape of a circle of radius one around the centre.

It's important to see that the decision boundary is a property of the hypothesis and not of the training set.

Suppose that we have polinomial terms with the for `xi^n xj^m`. That will give you even more complex decision boundaries.
