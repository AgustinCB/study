# Content cost function

The overall cost function is: `J(G) = \alpha * J[content](C,G) + \beta J[style](S, G)`. We will see now `J[content]`.

Say you use hidden layer l to compute content cost, chosen to be somewhere in the middle of pre-trained convolutional neural network.

Let `a[l](C)` and `a[l](G)` are similar, then both images have similar content. So we can define: `J[content](C,G) = 1/2 || a[l](C) - a[l](G) ||^2`.
