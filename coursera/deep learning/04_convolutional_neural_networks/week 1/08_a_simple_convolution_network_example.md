# A simple convolution network example

In this lesson we will see a concrete example of a deep convolutional network.

Let's say you have an image and you want to classify it as a cat or not. We will use small images of 39x39x3. The first layer uses a set of 3x3 filters, an stride of 1 and no padding. We also have 10 filters. So we have in the next layer an input of 37x37x10.

In the next layer we use 5x5 filters, a stride of two, no padding and 20 filters. Then the output of this (and input of the next layer) will be 17x17x20. In the last convolutional layer, we use also 5x5 filters, an stride of 2 and 40 filters. So we end up with a 7x7x40 volume.

At the end, we take that volume and make it a 1960 units vector that would go to a softmax unit, which will produce the the output of the neural network.

A lot of the work here is to set the hyperparameters. Typically, you start with large images (39x39) and gradually go down (7x7). While the number of channels tend to go up (from 3 to 40).

There's usually three types of layers in a convolutional network layer:

- Convolutional  layer (the ones we saw).
- Pooling layer.
- Fully connected layer.

Most convolutional neural network will have a little bit of all of them.
