# Examples and intuitions II

In this lesson we will continue with our example of the XOR operation after seeing how to compute the AND and OR operation.

Similarly, we can compute the NOT operation:

We will have two inputs (1, x[1]) and the parameters: `w = [10 -20]`, which will give us:

x[1] | h[\Theta](x)
 0   | g(10) around 1
 1   | g(-10) around 0

Or, also:

```
h[\Theta](x) = g(10 - 20 x[1])
```

Similarly, if we want to do `(NOT x[1]) AND (NOT x[2])`, we can do it by using something like: `m = [20 -30 -30]`.

With all this pieces together, we should be able to compute our XNOR model in a neural network.

We can use one hidden layers and one output layer. The first one, will have two nodes, the second one only one.

The first node of the hidden layer, will connect with all the inputs using the weights of the AND operation, the second using the weights for the `(NOT x[1]) AND (NOT x[2])` operation. 

The node in the output layer, will connect with 1 plus the output of the hidden layer using the weights of the OR operation. With that, we will have table of truth:

x[1] x[2] | a[2][1] a[2][2] | h[\Theta](x)
 0    0   |  0       1      | 1
 0    1   |  0       0      | 0
 1    0   |  0       0      | 0
 1    1   |  1       0      | 1

Which is the equivalent to the XNOR operation.
