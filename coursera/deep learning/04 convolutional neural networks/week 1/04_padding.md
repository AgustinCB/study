# Padding

In this lesson we will see how to use padding to our convolutional operation.

So we have an operation that is performed over an image of 6x6 and a filter of 3x3 and outputs a matrix of 4x4. More general, we can say that given an image of nxn and a filter fxf, our output will be `txt` where `t=n-f+1`.

So in general, our image will shrink and at the same time certain pixels will be used more often than others (the ones of the pixel would be used less often), throwing away a bunch of information.

There's a way to fix, you can "pad" the image. So image that you add a one pixel border to the image. Now you have an 8x8 image and you will receive as an output a 6x6 result. This way we didn't shrink the image and we're reducing the effect of counting less the pixels on the edges.

In general, given `p` as the size of the padding, now `t=n+2p-f+1`.

There often two choises on how to pad:

- In a valid convolution, you choose not to pad.
- In a same convolution, you choose to pad so that the output size is the same as the input size. This implies that `p=(f-1)/2`. By convention, `f` is usually odd. For two reasons: To simplify padding and to have a central position in the filter.
