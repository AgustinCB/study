# Softmax regression

For cases where you have more than one class in a neural network, you can use softmax.

Let's say that we want to recognize pictures that can be cats, dogs, baby chicks of other. Will say that cats is class 1, dogs 2, baby chicks 3 and other 0.

We will say `C` is a constant with the number of classes. We will create a neural network with `C` units on the output layer. Each unit will map to a class and output the probabily of that class given the sample. Their should all sum 1.

We will use then a softmax layer in the output layer:

```
Z[L] = w[L] a[L-1] + b[L]
a[L] = e.^z[L]/sum(1, C, j, e.^z[L][j])
```

This activation function takes an array of elements and outputs an array of elements.
