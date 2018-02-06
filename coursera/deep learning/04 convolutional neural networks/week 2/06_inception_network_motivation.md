# Inception network motivation

In this lesson we will see how inception networks work by applying all kind of layers. It was introduced in Szegedy et al. 2014.

Let's say that you have a 28x28x192 volume. What an inception network does is apply everything.

So you try a 1x1 filter to get a 28x28x64 result, a 3x3 filter to get a 28x28x128, a 5x5 filter to get a 28x28x32 and max pool (with same padding so dimensions match) resulting in 28x28x32.

You'd stack all that together into a big a volume of 28x28x256. This is called an inception module.

There's a problem, though: Computational cost.

Let's just focus on the 5x5 same convolution of 32 filters. The computational cost of this step is:

You have 32 filters of 5x5x192. So we are performing 28x28x32x5x5x192=120M operations. While you can do this in modern computers, this is still very expensive.

Now let's try the same using a 1x1 convolution to reduce to 16 channels and then run the 5x5 convolution with 32 filters.

So we're first shrinking the volume into an smaller one before performing the operation (this intermediate volume is often called bottleneck layer).

So you'd be doing 28x28x16x192=2.4M in the first step. In the second step it would be 28x28x32x5x5x16=10M. So in total, we need to do about 12.4M. We reduced the computational cost ten times!
