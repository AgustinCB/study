# Classic networks

In this lesson we will learn about the classic neural network.

## LeNet-5 (LeCun et Al., 1998)

Its goal was to recognize hand-written digits from 32x32x1 images.

In the first layer we use fix filters of 5x5 with an stride of one and no padding. 

Then it did average pooling using a filter of two and a stride of two.

Then another convolutional layer with 16 filters of 5x5, stride of one and no padding.

Then another pooling layer as the previous one.

The next layer is then a fully connected layer with 120 units, followed by another fully connected layer of 84 units followed by a softmax classifier with ten units.

This had 60k parameters. Which is pretty small for today's standards.

Advanced comments:

It used sigmoid and tanh mostly.

It used to use filters that would look at different channels (instead of one per channel) to save on parameters.

It also had a non-linearity after pooling (sigmoid).

## AlexNet (Krizhevsky et al., 2012)

It starts with a 227x227x3 images. Firt it applies 96 11x11 filters of stride four and no padding. Then applies Max Pooling with a 3x3 filter with stride of two.

It does then a 5x5 same convolution with 256 filters. Then a max pooling with the same parameters than before. Then three steps of 3x3 with same convolution with 384, 384 and 256 channels each time and then a max pooling as before.

It finishes with three fully connected of 9216, 4096 and 4096 respectively, finishing with softmax and 1000 units.

Similar to LaNet, but much bigger (about 60M parameters).
It used ReLU.
It had a complicated way to train in multiple GPUs.
It had another layer called Local Response Normalization (LRN): It would look at one position against all channels and normalize them. This was found that it doesn't help that much and isn't really used in practice that much.

## VGG - 16 (Simonyan & Zisserman 2015)

It would have just convolutional layers of 3x3 filters, same padding and stride of two followed by max pooling of 2x2 and stride of two.

It'd start with 224x224x3 images and apply two convolution layers with 64 filters one after the other. Then uses a pooling layer. Then has two other convolution layers with 128 filters. Then a pooling layer. Then three convolutional layers with 256 filters and then a pool layer. Then 3 convolutional layers of 521 followed by one poo layer. Followed by three 512 convolutional layers followed by pooling layer.

It would finish with two fully connected layers of 4098 and Softmax of 1000.

It uses about 138M, which is a lot even for today's standards. But it's very simple and elegant.
