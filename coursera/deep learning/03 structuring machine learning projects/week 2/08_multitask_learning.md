# Multitask learning

In this lesson we will see multi-tsak learning, we will see how to start multiple neural networks doing tasks at the same time that by themself will complete a different task.

Let's say that we are building an autonomous driving system and we want to identify objects in the neural network. For example, if there's pedestrians, if there's cars, if there's stop signs or if there's traffic lights.

We can think of this as function with an image as an input and a four integers array as output, where every value would be zero or one depending on whether there was the element that that value maps to.

We need to define now the loss of the neural neutwork: `1/m sum(1, m, i, sum(1, 4, j, l(y'[i][j], y[i][j])))`, having `l(y', y) = -y log(y') - (1-y) log(1 - y')`. The difference of this with softmax is that now one image can have multiple labels.

If you train a neural network to optimize this cost function, you're carrying multi-task learning. This results in better performance than training four different networks sepparately, because they'll share the shared knowledge.

In cases where your dataset isn't completely labeled, you'd just omit the values that aren't present in the sum.

Multi task learning makes sense when:

1. Training on a set of tasks that could benefit from having shared lower-level features.
2. The amount of data you have for each task is quite similar (this isn't really a rule but a correlation).
3. You can train a big enough neural network to do well on all the tasks.
