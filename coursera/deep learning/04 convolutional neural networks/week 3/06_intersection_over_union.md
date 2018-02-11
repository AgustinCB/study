# Intersection over union

In this lesson we will see how to tell if your object detection algorithm is doing well using the function intersection over union.

In object detection your expected to localise the object too. Suppose that we have a possible location and the actual answer. We would compute the intersection and the union of them. We would divide the intersection between the union area and so long as the result is greater or equals to 0.5, we would say that the answer is correct. 0.5 is a convention, you could use whatever better fits your problem.

More generally, IoU is a mesure of the overlap between two bounding boxes. The bigger, the more similar two boxes are together.
