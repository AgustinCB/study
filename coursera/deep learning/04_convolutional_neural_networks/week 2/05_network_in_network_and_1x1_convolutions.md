# Network in network and 1x1 convolutions

In this lesson we will see what's a 1x1 convolution and how to use it.

Let's suppose that you have a 1x1 filter and you convolve it to a 6x6 images. It's like doing an element wise operations. But that's true only if you have one channel.

If you have more than one channel, it'd apply a ReLU over the multiplication of multiple numbers.

It's basically having a fully connected neural network that applies ReLU outputing the number of filters. It's often called Network in network and is described in Lin at al., 2013.

Suppose you have a 28x28x192 volume. If you want to shrink the number of channels, you can use 32 1x1x192 filters and you get a 28x28x32 volume as a result.

It also adds non-liniarity.
