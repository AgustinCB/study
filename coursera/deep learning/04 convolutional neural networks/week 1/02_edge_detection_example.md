# Edge detection example

In this lesson we will see how to implement convolution operation from an example.

Let's say that we have a neural network to detect edges in a picture. How do we do so?

Suppose that we have a grey scale image of 6x6 pixels and a filter matrix of the value `[[1 1 1] [0 0 0] [-1 -1 -1]]` and pass them through the convolution operation (we will call it asterisq) to produce a 4x4 matrix.

To compute the value in 1x1, we will multiple the first submatrix 3x3 of the image for the values on the filter element wise and them sum all together. For the element in 1x2, we will do the same but using the submatrix 3x3 of the image that starts in 1x2. And so on. This turns out to be an edge detection!

To ilustrate this lest's use a simplified case in which we have a matrix with three columns of tens first and the three columns of 0. As an image, you will have a very clear edge in the middle.

When combinated with our filter, you'd get the matrix `[[0 0 0 0] [30 30 30 30] [30 30 30 30] [0 0 0 0]]`. If you plot this image, you'll have a very big white column in the middle.

So we can think of a vertical edge as 3 by 3 region where there're bright pixels on the left and dark pixels on the right.
