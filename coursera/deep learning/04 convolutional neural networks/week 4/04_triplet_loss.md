# Triplet loss

In this lesson we will see how to learn the parameters if your neural network to compare images, we will use the triplet loss function. It was explained in Schroff et al., 2015.

Given two images that are similar, you want the encoding of your image to be similar. If they are not, you want it to be very different.

You'd usually have an "anchor" image and a "positive" or "negative" image (depending on whether there's a match or not).

So we want that: `||f(A) - f(P)||^2 <= ||f(A) - F(N)||^2`, where `d(A, P/N) = ||f(A) - f(P/N)||^2`.

So we have that: `||f(A) - f(P)||^2 - ||f(A) - f(N)||^2 <= 0`.

To prevent the image to just return zero for everything, we will modify the condition to be `||f(A) - f(P)||^2 - ||f(A) - f(N)||^2 + \alpha <= 0`.

`\alpha` pushes the (anchor, positive) pair and the (anchor, negative) pair further away.

Given three images A (anchor), P (positive) and N (negative), we will define the loss as follows: `l(A,P,N) = max(||f(A) - f(P)||^2 - ||f(A) - f(N)||^2 + \alpha, 0)`. Our objetive would be to minimise this function. So:

`J = sum(1, m, i, l(A[i], P[i], N[i]))`. So our training set can be 10k pictures of 1k persons in which you have multiple pictures of the same person.

Choosing A,P,N randomly then `d(A,P)+\alpha <= d(A,N)` is easily satisfied. So to construct your NN you want to choose triplets that're hard to train on, i.e., cases where the loss of every pair is very close.
