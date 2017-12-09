# Neural network representation

In this lesson, we will cover how to interpret the pictures that we draw about neural networks.

## Neural neetworks with a single hidden layer

Suppose that we have three inputs. That's called the input layer of the neural network.

Then we would have another layer. A hidden layer.

The final layer will formed by one form, the output layer, that will generate the predicted value.

In a neural network that you train with supervised learnin, the training set contains sets of the inputs x with the output values y. So the term hidden layer refers to the fact that output values of those layers are unseen in the training set.

We would call the input layer as a[0]. a in this context, means activation. The hidden layer will create a set of activations, a[i], where i is the 1-index of the layer. Each a[i] is a vector with a cardinality equals to the number of units in the layer. The output layer, a[l], where l is the number of layers, will be equals to the output, y'.

If you have one hidden layer and one ouput layer, we would have a two layer neural network. We don't count the input layer.

Hidden and output layers have parameters (weights and bias units) associated with them. We represent them as matrixes for the weights and vectors for the bias units.
