# Activation functions

When you create a neural network, one of the choices you get to make is which activation function to use in your units. The most common case if the sigmoid function, but sometimes other functions make more sense.

sigmoid(x) = 1/(1+e^-x)

Another possibility is a non-linear function that isn't sigmoid, for example the hyperbolic tangent function:

tanh(x) = (e^x - e^-x)/(e^x + e^-x)

This function almost always work better than the sigmoid function. It keeps the mean of the outpus close to zero, which is more useful. The only exception is the output layer, in which you want a function constraint to 0 and 1, while tanh gives output from -1 to 1. It's usual to use the tanh function for the hidden function and sigmoid for the output layer.

On of the problems of this function is if x is very small or very large, the slope is close to zero and gradient descent becames small. In that case, you'd something like this:

ReLU(x) = max(0,x)

In general, if your output is between 0 and 1, you'd use sigmoid in the output layer and ReLU in the hidden layers. The advantage of ReLU is that the slope of the activation function is that there's less chances to get an slope of zero and therefore the gradient descent is faster.

To recap:

Sigmoid: Only use it for output layers when you have classification problems.
tanh: Use it as a better alternative to sigmoid.
ReLU (and leaky ReLU): Should be your default choice of activation function.

The leaky ReLU is: g(x) = max(0.01 x, x)
