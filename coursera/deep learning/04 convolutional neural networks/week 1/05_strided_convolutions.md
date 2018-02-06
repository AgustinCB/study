# Strided convolutions

In this lesson we will see how what are strided convolutions.

Let's say that we have an image of 7x7 and an image of 3x3 and we want to convolute them together. But instead of doing the usual way, we would use a stride of 2. That means that in every iteration, we take two steps instead of one, skipping over one column or row after performing an operation in a submatrix. That would produce a new matrix of 3x3. So now `t=(n+2p-f)/s+1` where `s` is the stride, rounding the result down to the floor of the result.

As a technical note on cross-correlation vs convolution:

In a math textbook, a convolution filter needs to be first flipped by the horizontal and vertical axis. Technically, what we use here is called cross-correlation and not convolution. Although by convention, in ML literature, it's just called convolution operation.

The mathematical convolution has a nice property: `(A * B) * C = A * (B * C)`.
