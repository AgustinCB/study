# Multi-class classification

In this lesson, we will cover how to do multiclass classification with neural networks.

Let's say that we have a neural network that will receive an image and decide if it's a pedestrian, a car, a motorcycle or a truck.

In that case, we will have four output nodes, one for every class. And we want to have only one returning one and the rest of the output nodes return zero. That way, contrary to our previous attempt, we won't represent y as {1,2,3,4} but as a four elements arrays of zeros and ones.
