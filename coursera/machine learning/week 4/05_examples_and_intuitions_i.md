# Examples and intuitions I

In this lesson we will explore an example on how to use neural networks.

## Non-linear classification example: XOR/XNOR

Considere this example: We have two input features that binary values, i.e. either 0 or 1. 

We want a non-linear hypothesis that will separate the possitive and the negative examples (possitive meaning same values in each input, negative different).

We want to compute this:

```
y = x[1] XNOR x[2] = NOT (x[1] XOR x[2])
```

To achieve this, we will start with a simpler example: The AND function.

### AND function

```
x[1], x[2] = {0, 1}
y = x[1] AND  x[2]
```

We will have a neural network with three nodes on the input layer (1, x[1] and x[2]), one node in the output layer.

We will use the weights `w = [-30 20 20]`. Which means that we will multiply 1 with -30, x[1] with 20 and x[2] with 20. So:

```
h[\Theta](x) = g(-30 + 20 * x[1] + 20 * x[2])
```

We will have then:

```
h[\Theta](0) = 0.5
h[\Theta](4.0) = 0.99
h[\Theta](-4.0) = 0.01
```

Let's look at the outputs depending of x[1] and x[2]:

x[1] | x[2] | h[\Theta](x)
 0   |  0   | g(-30) around 0
 0   |  1   | g(-10) around 0
 1   |  0   | g(-10) around 0
 1   |  1   | g(10) around 1

Which has the same form as the truth table of the AND operation.

In the same way, if we were using `w = [-10 20 20]`, you will be getting the OR operation.
