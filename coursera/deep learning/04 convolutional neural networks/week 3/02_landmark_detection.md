# Landmark detection

In this lesson we will see how a neural network can output landmarks.

Let's say that you're building a face recognition application and you want to also know the corners of the person eyes', you would add the following variables: `l1x, l1y, l2x, l2y, l3x, l3y, l4x, l4y`.

You may define an arbitrary list of landmarks in the face and use that to make the algorithm tell you where are the strategic places of the face.

As before, you would have a ConvNet and then an output layer that will have a vector with the results (plus the first bit to detect whether there is a face or not at all).

Using this, you may be able to recognise things like emotions or apply filters.

In order to train an algorithm like this, you need a training set in which someone anotated the landmarks.
