# Model Representation I

Now we will discuss how to represent our hypothesis in terms of neural networks.

## How does a neuron look like.

Neurons have a central called body with a kernel called "nucleus" and input wires called "dendrite." These receive inputs from other locations.

It also has "output wires" called "axon" and that's what it uses to send signals to other neurons.

So it basically is a system that receives some information through the dendrite, it processes them in the nucleus and outputs them throught the axon. 

They usually work in groups and communicate through electronic pulses, by connection the axons of the sending neuron to the dendrite of the receiving neuron. This is the process by which all human thoughts happen.

In an artifical neural network, we will do a simpler version of that by representing a neuron as a logistic unit that takes a set of inputs and returns an output. We will represent all this process through the following mathematical expression (already discussed):

```
h[\theta](x) = 1 / (1 + e ^ (\theta'x))
```

Where `x` and `\theta` are parameter vectors.

as usual, we may add a new input value, often called "bias unit" and under the index `0`, with the value one to simplify operations.

This is often named as a sigmoid (logistic) activation function. We call \theta (the parameters of the model) are often called the weights of the model.

A neural network will be a group of units like this working together in layers.

We will have an input layer that will connect each node to different nodes in the next layer (the first "hidden layer"), which will pass that through the sigmoid function and send it to the following layer.

That can be another hidden layer or the output layer (the last one), formed by a single unit that will output the result.

Some notation:

`a[i][j]` => Activation of unit `i` in layer `j`
`\Theta[j]` => Matrix of weights controlling function mapping from layer j to layer j + 1. For example, we can have the following neural network:

```
a[1][2] = g(\Theta[1][10] * x[0] + \Theta[1][11] * x[1] + \Theta[1][12] * x[2] + \Theta[1][13] * x[3])
a[2][2] = g(\Theta[1][20] * x[0] + \Theta[1][21] * x[1] + \Theta[1][22] * x[2] + \Theta[1][23] * x[3])
a[3][2] = g(\Theta[1][30] * x[0] + \Theta[1][31] * x[1] + \Theta[1][32] * x[2] + \Theta[1][33] * x[3])
h[\Theta](x) = a[1][3] = g(\Theta[2][10] * a[2][0] + \Theta[2][11] * a[1][2] + \Theta[2][12] * a[2][2] + \Theta[2][13] * a[2][3])
```

Which represents a three layers network with three inputs, three nodes in the hidden layer and one output layer. As you can see, here \Theta[i] is in R(3x4) and more generally, if a network has `s[j]` units in layer j, `s[j+1]` in layer `j+1`, then `\Theta[j]` will be of dimension `s[j+1] x (s[j] + 1)` (because of the bias unit).
