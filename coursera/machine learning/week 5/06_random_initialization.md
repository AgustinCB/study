# Random initialization

When running an algorithm like gradient descent, we need to pass an initial value. We usually set it to zero.

This is ok when using logistic regression, but won't work while training a neural network. That will cause that all the values of the nodes in a hidden layers will be the same, the \delta value of the backpropagation will also be the same. Not only that, but after each update, the values of the weights of the inputs going into each hidden units are identical. That means that the neural network won't be able to compute interesting functions.

To avoid this, we use something called random initialization: Initialize each value of `\Theta`to a random value in `[-\epsilon, \epsilon]`.
