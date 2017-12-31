# Understanding mini-batch gradient descent

In this lesson we will go into details on how to implement mini-batch gradient descent and get a better understand on what's doing and why it works.

With batch gradient descent, you expect the cost function to go down over the number of iterations. Mini-batch gradient descent, on the other hand, doesn't have this property and the cost function may go up and down all over the place, since every iteration is training in a different set. However, even with the noice, it should trend downwards.

One of the parameters you have to choose is the size of the batch. If the mini batch size is equals to m, then we are doing batch gradient descent. If the mini batch size is equals to one, then we have an algorithm called stochastic gradient descent and here every example is its own mini-batch. Batch gradient descent would be able to take very low noice steps. Stochastic gradient descent will be the opposite: It will have a lot of noice and although it will always go to the center, it will never converge.

In practice, the size will be somewhere in between. Batch gradient descent takes too long per iteration, stochastic gradient descent will lose all your speedup from vectorization, since you're processing one single sample at the time. A value in between won't take that long per iteration (and make progress on every iteration) and will be able to take advantage of vectorization.You aren't guaranteed to converge in the minimum (it depends on the learning rate), but it will go more directly than stochastic gradient descent.

If you have an small training set (< 1000): Use batch gradient descent.

Otherwise, typical mini-batch size would be a power of two between 64 and 512. Make sure that your mini-batches fit in CPU/GPU memory.

In practice, you'll try idfferent values and pick the one that makes the gradient descent algorithm as efficient as possible.
