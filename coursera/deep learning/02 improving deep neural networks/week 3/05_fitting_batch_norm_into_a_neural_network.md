# Fitting Batch Norm into a neural network

In this lesson we will see how to implement batch norm into a neural network.

As seen before, each unit in a neural netwokr has two steps of computation: First to calculate z, second to calculate a.

In the first layer, to compute batch norm, you would calculate z from the input x, apply batch norm with the parameters \beta and \gamma from the previous layer and pass the result to the activation function to get the output.

For subsequent layers, you'd grab the output of the previous layer doing the same process over them that we did over the the input of the neural network in the first layer.

So as parameters for that layer you will have the usual w and b and the new vectors \beta and \gamma. You will treat them exactly the same as w and b, even optimizing them using gradient descent.

They're usually trained using mini-batches. Using batch norm may make the use of b pointless, you can usually omite them since \beta ends up having the same effect.

So your gradient descent would look like:

```
for t = 1...NumBatches
  compute forward prop on X[t]
    for each hidden layer, use Batch norm over their z value.
  use backprop to compute dW[l], dBeta[l], dGamma[l]
  Update parameters
```

This works with momentum, RMSprop, Adam and other techniques we already saw.
