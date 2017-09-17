# Non-linear hypotheses

We will cover now Neural Networks. The idea behind them is to able to learn from complex, non-linear hypothesis.

Suppose that we want to predict whether a house will be sold within the first six months or not. That's a classification problem. And suppose that we have large number of features to work with, let's say 100. Also suppose that the shape of the hypothesis is highly unconventional.

We could try to fit our data using second order features and add things like: `x[1]2, x[1]*x[2], x[1] * x[3]...`, but that will increase the number of features (roughly 5000) and will increase the chances of overfitting.

We could also reduce the number of new features by, for example, only using the quadratic form of the current features. However, that won't let us fit curious and complex hyphotesis, like the ones with a circle shape.

We need something different for this (i.e. complexy hypotheses with a high amount of features)
