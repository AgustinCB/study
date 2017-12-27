# Understanding dropout

In this week we will explain why dropout works a regularizer.

Intuition: Can't rely on any one feature, so it has to spread out weights. That happens because a unit can be randomly eliminated. So it will be reluctant to put too much weight on one unit, because it may go away. That will have the effect of shrinking the weights. It's even possible to show that dropout has a similar effect to L2 regularization.

It's also possible to change the hyperparameter `keep_prob` by layer, letting you adjust it depending on whether you worry or not about overfitting in a particular layer. You can even apply the algorithm to the input layer, although that's rarely useful in practice and often it has a very high value on `keep_prob`.

It's often used in computer vision.

One big downside of dropout is that the cost function J isn't well defined anymore, because it changes in every iteration and that looses some debugging tools.
