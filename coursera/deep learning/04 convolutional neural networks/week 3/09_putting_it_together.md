# Putting it together

In this lesson we will see how to put together all the components together to form the YOLO algorithm.

Suppose you're training an algorithm to detect pedestrian, car and motorcycle. You want to use two anchor boxes (one for vertical boxes, another one for horizontal boxes) and nine grid cells divided in a matrix of 3x3. So your output volume would be 3x3x2x8 (or 3x3x16).

Then you train a ConvNet that takes an image and outputs this volume.

Given an output, you'd have possible bounding boxes. If the value of Pc is not zero, you will have an object.

Then you'd run non-max supressed outputs: For each grid cell, get the two predicted bounding boxes. Get rid of the low probability predictions. For each of the three class use non-max suppression to generate final predictions.
