# Other regularization methods

In this lesson we will see other techniques to reduce overfitting in algorithms.

- Data augmentation: You can modify your samples in your training set to increase the size of your training set. For example, if you have a datasets of images, you can rotate them and double the size of the training set.

- Early stopping: As you run gradient descent, you plot the training error over the number of iterations. You'd also plot the dev set error. You'll find that the later will go down for a while and then increase. We can then stop the gradient descent at the point in which dev set error is lower. It has a downside, though: It reduces the isolation between not overfitting and optimizing cost function (called orthogonalization). Early stopping couples this two tasks and you can't work in them independently anymore.
