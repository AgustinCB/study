# Putting it together

In this lesson, we will put all the pieces together to have a big picture on how to implement a neural network.

First, you need to know the network architecture (connectivity pattern between neurons). To decide the number of input units you just need the dimension of features. The number of output units will be the number of classes.

As of the number of hidden layer, a reasonable default is to use one hidden layer. If you have more than one hidden layer, a reasonable default is to have the same number of hidden units in every layer (usually the more the better, but keeping in mind that that increases the computation complexity). Is also useful to have a number of units per layer bigger than the number of inputs.

## Training a neural network

1. Randomly initialize the weights.
2. Implement forward propagation to get `h[\Theta](x[i])` for any `x[i]`
3. Implement code to compute cost function
4. Implement backprop to compute partial derivatives.

We do it as of: For each training set, we perform forward and backward propagation using the values (getting the activation value and the delta terms for each layer).

At the end, we compute the partial derivative terms.

5. Use gradient checking to compare the partial derivative terms computed usin backpropagation versus using numerical estimate of gradient of `J(\Theta)`. Then disable gradient checking.
6. Use gradient descent or advanced optimization method with backpropagation to try minimize `J(\Theta)` as a function of parameters `\Theta`. Also `J(\Theta)` is usually non-convex and may not arrive to a global optimum (although that's strange it often gives good results).
