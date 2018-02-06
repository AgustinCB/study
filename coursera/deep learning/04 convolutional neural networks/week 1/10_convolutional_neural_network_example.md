# Convolutional neural network example

Let's see an example of a convolutional neural network.

Let's say that we are trying to do hand writting digit recognition for images of 32x32 and three colors.

Let's say that the first step uses a 5x5 filter and an stride of 1, applying six filter. So that give us a 28x28x6. We call this CONV1.

For the next step, we apply Max Pooling with a 2x2 filter and a stride of 2. So we have now 14x14x6 as a result. We call this POOL1.

There's no convention on whether POOL1 is a layer by itself or not, for the purposes of this example, we will call layers only steps that have weights and use CONV1 and POOL1 as one layer (called layer 1).

Next, let's have convolutional step with 16 5x5 filters and an stride of one called CONV2 (with an output of 10x10x16) with another Max Pooling of filter size two and stride two. So the output of this whole layer will be 5x5x16. All this is layer 2.

Now we will fatten the output of this layer into a 400 dimensions vector and we will have an standard layer that will output 120 units. We will call this FC3 (fully connected layer).

We will do the same again to reduce it 84 and call it FC4.

Finally, we will have a softmax layer with ten outputs.

Usually to choose hyperparameters you'd read literature to see what worked well for others in the past in similar problems.

This represents a ver common pattern seen in neural networks:

CONV -> POOL -> CONV -> POOL -> FC -> FC -> Softmax.
