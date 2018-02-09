# Convolutional implementation of sliding windows

In this lesson we will learn how to implement sliding windows using convolutional layers.

First, we will see how to torun a FC layer into convolutional layers.

Let's say that you have a 14x14x3 image, then a layer with 16 filters of 5x5, the a max pool layer of size 2, then two fully connected layers with 400 units and then softmax with four classes.

We could implement the first fully connected layer by using 400 filter. Because the input of that layer would be 5x5x15, the output dimension would be 1x1x400. Mathematically, this is the same as a fully connected layer.

For the next FC, we would implement a 1x1 convolution with 400 filters, which would give us the another layer that outputs 1x1x400. For softmax, we would just grab four 1x1 filters and use softmax as the activation function.

Sermanet et al., 2014 explained how to do the same for sliding windows:

Suppose we have the previously mentioned architecture.

Let's say that we expect images of 16x16x3. Running an sliding window over this to run in our architecture, we need to run a sliding window with size of 14x14 and pass every step there.

However, we could just pass that through the neural network. The sizes would be slightly different:

After layer 1, you have 12x12x16 instead of 10x10x16.
After layer 2, you have 6x6x16 instead of 5x5x16.
After layer 3, you have 2x2x400 instead of 1x1x400.
After layer 4, you have 2x2x400 instead of 1x1x400.
After layer 4, you have 2x2x4 instead of 1x1x4.

The output would be a volume in which every row matches with one of the sliding windowis.
