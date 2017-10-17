# Deciding what to do next (revisited)

In this lesson we will cover what to do once to do to improve the performance of our algorithm based.

Suppose you have implemented regularized linear regression to predict housing prices. However, when you test your hypothesis in a new set of houses, you find that it makes unacceptably large errors in its prediction. What should you try next?

- Get more samples. Helps to fix high variance.
- Try smaller sets of features. Fixes high variance
- Try getting additional features. It sometimes fix a high bias problem.
- Try adding polynomial features. It sometimes fix a high bias problem.
- Try decreasing \lambda. It helps to fix a high bias problem.
- Try increasing \lambda. It helps fix a high variance problem.

## Apply it to neural networks

On small neural networs. With parameters you're more prone to underfiting. It's also computationally cheaper.

Large neural network tend to be more prone to overfitting and computationally more expensive. However, using regularization to address the problem is often more effective than using an small neural network.

To decide the number of layers to use, you can use a similar trick that the ones we discussed: You can try different number of layers with your training set, select the one with smaller error value in the validation set and see it's actual error with the test set.
