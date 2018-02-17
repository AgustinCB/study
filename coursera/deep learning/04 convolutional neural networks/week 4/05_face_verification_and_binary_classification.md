# Face verification and binary classification

Another way to learn the parameters for the syamese network is with binary classification.

Once we have the representation vectors of both images, we would feed it to a binary classification unit that would output 1 or 0 depending on whether the images are from the same person or not. We can write then:

`y' = g(sum(1, 128, k, w[i] |f(x[i])[k] - f(x[j])[k]| + b))`.

Where 128 is the size of the output vector of every image.

You can also precompute the encoding of the anchors and save time.

To train it you use pairs (and not triplets) with 0 and 1 as outputs.
