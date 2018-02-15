# Siamese network

The job of the function `d` is to input two faces and tell you how different or similar they are. A way to do this is using a siamese network, introduced in Taigman et al., 2014.

Let's say that we have a CNN that accepts an image and gives some input. Normally, before the input there would be FC layer that spits a vector. That vector is called "encoding of x[i]," where x[i] is one sample. You can run two images through this network and if you believe those encodings are good representations of the images, then we can define `d(x[i], x[j]) = norm(f[i] - f[j])`.

How do you train this siamese neural network? The parameters of the NN that defines the encoding `f(x[i])` should be so that if x[i] and x[j] are the same person, then the norm of the difference of the vectors is small and large otherwise.
