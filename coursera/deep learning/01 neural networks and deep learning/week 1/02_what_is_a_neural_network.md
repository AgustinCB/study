# What is a neural network

The term deep learning refers to train very large neural networks. In this lesson we will cover the basic intuitions around neural networks.

## Housing price prediction

Let's say you have a dataset with six houses, for which you have the size and the price. You want to create a function that predicts the price from the size. If you're familiar with linear regression, you may feel tempted to use some kind of linear interpolation system. This is, indeed, the simplest possible neural network:

```
size (x) -> neuron -> price (y)
```

We know that the price of the house can't be negative, so the function that `neuron` applies will start at zero for some values of x and eventually start increasing. This is fairly common shape of a function in neural networks and is called ReLU function (rectified linear unit).

If this is the simplest neural network, you can construct more complex ones by "stacking" more neurons together.

Let's say that instead of predicting the price from the size, we also have the number of bedrooms. We can use those two features to get the family size of the house. We also have the zip code, from which we will have the walkability of the neibourhood. If you also have the wealth, you can also get the scool quality. We can represent that as a neural network:

```
size + bedorooms -> neuron -> family size
zipcode -> neuron -> Walkability
zipcode + wealth -> neuron -> school quality
family + walkability + school quality -> neuron -> price
```

By stacking together a few neurons, we have now a complex system that will be able to figure out what features are important. This is a neural network with four input values, one output and three hidden units. When all the inputs are connected to all the hidden units, we say that the neural network is densely connected. A remarkable thing about neural networks is that given enough samples of x and y, they often perform really well on prediction future ys based on new inputs.
