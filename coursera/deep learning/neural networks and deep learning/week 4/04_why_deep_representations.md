# Why deep representations?

In this lesson, we will get some intuitions on why deep networks work well.

Suppose that you are building a system for face regconition. Then the first layer of the neural network may be thought as an edge recognition. Then the second layer would put together lots of edges and detect parts of faces. Finally, the third one would put together part of faces for composing complex and full faces. This will make more sense when talking about convolutional neural networks, but for now you can think them as going for very specific and simple representation to more general and complex one.

The same applies to other neural networks that try to do audio recognition.

The other piece of intuition come from circuit theory:

There are functions you can compute with a small L-layer deep neural network that shallower networks require exponentially more hidden units to compute.

Let's say that we are trying to compute the exclusive or the parity of a bunch of features in which you pair inputs, pass them to xor units and then pair the outputs of that and pass it through xor units and so on still you only have one.

If you aren't allowed to use multiple hidden layers and you have to use only one hidden layer, then the number of units will be exponentially large (in this case about 2^(n-1) units).
