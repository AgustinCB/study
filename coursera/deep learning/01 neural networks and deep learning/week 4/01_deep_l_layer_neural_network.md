# Deep L-Layer neural network

In this lesson we will see how to put together all we know so far together to build a deep neural network.

## What is a deep neural network

We have seen logistic regression and with one hidden layer. We say that logistic regression is a "shallow" model and one with five hidden layers a "deeper" model.

Suppose a four layer neural network with five, five, three and one units per layer. We will use the following notation:

L = number of layers
n[l] = number of units in layer l
n[0] = nx = number of inputs
a[l] = the activations in layer l (vector of size n[l])
x = a[0] = inputs
W[l] = weights at layer l
b[l] = bias at layer l
z[l] = z[l] * a[l-1] + b[l]
