# Convolutions over volumes

In this lesson we will see how to implement convolutions over three dimensional volumes.

Let's say that we want to detect edges in a RGB image instead of a greyscale image. So we have a "three layer" image of 6x6x3. We will also have a filter with three layers of dimension 3x3x3. Where the first number is the height, the second is your width and the third is the number of channels. The number of channels in both the image and the filter should be equal. The output will be a simple 4x4 image.

To compute this operation we will first grab our 3x3x3 filter and place them in the position 1x1x1 of the image and multiply them element wise for their corresponding 27 numbers, summing the result. The rest of the process maps directly with the conventional convolution operation.

For example if you had your edge detector in the red channel of the filter and zero matrixes in the green and blue, you'd identify edges in the red channel. You can also put it in every channel and detect it in every channel.

Now that you know this, what if you want to use multiple filters at the same time?

Suppose that you have two filters, you can stack the output of both filters into one 4x4x2 result, where the last number is the number of filters applied.
