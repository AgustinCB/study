# Computing a neural network output

In this lesson we will cover the details of how a neural network computes its output.

Let's assume a two layers neurla network with four units in the hidden layer.

Each node in the hidden layer does two steps of computation:

1. It computes `z[1][i] = w[1][i]' x + b[1][i]`
2. It computes `a[1][i] = sigmoid(z[1][i])`

Where i is the unit number. The one used as index represents the index of the hidden layer.

`w[1][i]` is a vector and `b[1][i]` is a value.

We can represent all this as a matrix operations by defining the matrix W as the stack of all the `w[1]` transposed vectors and, X vector as the single column matrix formed by stacking the inputs and the B vector as the single column matrix formed by stacking the `b[1][i]` values and then:

Z[1] = W * X + B

Where Z[1] is a single column matrix formed by stacking all the `z[1][i]` values. We can then define the A[1] single column matrix as `sigmoid(Z[1])`.

Then we can define `Z[2]` as `W[2] A[1] + b[2]` and `A[2]` as `sigmoid(Z[2])`. The last output unit, is lot like a classic logistic regression.
