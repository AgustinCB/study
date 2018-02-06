# Pooling layers

In this lesson, we will see how conv nets use pooling layers to reduce the size of the information to speed computation as well as make some of the features they detect more robust.

Suppose you have a 4x4 input and you want to apply a type of layer called Max Pooling to get a 2x2 output. To do so, you will divide the input in 2x2=4 regions and take the max of each of it. You'd use those to construct the output. This has the hyperparameters of `f=2` (filter size) and `s=2` (stride). Same formulas as before apply.

A high number represent a feature detected. What Max pooling does is to preserve the feature over noise. Interestingly, it has no parameter to learn.

Another alternative is average pooling, in which you put the average of the region.

It's often used to collapse your representation.
