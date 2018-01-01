# Tuning process

In this lesson we will see how to go about how to systematically organize your hyperparameter tuning.

Some of the hyperparameters that you will want to tune are:

- Alpha 1
- Momentum term 2
- Adam's parameters (beta1, beta2, epsilon) 4
- Number of layers 3
- Number of hidden units 2
- Learning rate decay 3
- Mini-batch size 2

The lower the number the most important to tune.

How do you select a set of hyperparameters to explore?

In the early days of machine learning, you'd try a bunch of combinations and choose the best performing. This works fine if the number of hyperparameters is small. Today, you would choose them at random and test them. This works very well because you don't necessarily know which one is more important and this gives you the chance to try a lot of different values and you're more richely exploring values.

After doing a core sample of your sample, you may have an idea of the ranges that seem to work better. Then you can create a new sample focused on that ranges to tune even more. This is the core defined search process.
