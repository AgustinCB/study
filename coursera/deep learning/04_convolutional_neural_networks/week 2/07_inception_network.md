# Inception network

In this lesson we will see how to build an inception network from inception blocks.

Let's say that we have a volume of 28x28x192 and we have an inception block with:

- a 1x1 same conv layer with 16 channels followed by a 5x5 same conv with 32 channel
- a 1x1 same conv layer with 96 filters followed by a 3x3 same conv with 128 channels
- a 1x1 same conv layer
- a Max Pool layer with same padding f=3. This will have a lot of channels (192), so we will add another 1x1 same conv layer with 32 filters.

After that, we will have a layer called channel concat that will concatenate the output of each layer into a big volume


What an inception network does is putting a lot ofthis blocks together.

There's also some side branches in inception networks:

They take some hidden layer and try to make a prediction. It helps ensure that the features computed in the hidden layers aren't too bad to compute the output.
