# Why convolutions?

Let's see why convolutions are useful and how to train convolutional neural network.

They have this main advantages:

- Parameter sharing.
- Sparsity of connection.

Let's see an example:

Let's say that we have a 32x32x3 image and we use six 5x5 filters and stride or one to get a 28x28x6 output. That's we passed from 3072 units to 4704. If were using a normal neural network, we would need 3072x4704 parameters to learn, near 14M. However, we only needed 6x26=156 paramegers to get this.

There're some reasons for this:

- Parameter sharing: A feature detector that's useful in one part of the image is probably useful in another part of the image. So we reduce redundancy in parameters.

- Sparsity of connections: In each layer, each output value depends only on a small number of inputs.
