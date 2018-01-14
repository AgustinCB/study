# Avoidable bias

In this lesson we will see how well we want our training error to be.

Let's say that we have a cat classifier algorithm with 8% error on the training set and 10% on the dev error. We know that a human would perform the task with a 1% accuracy.

Because there's a huge gap between the traning set error and the human error, we know that there's a huge room for improvement on that. We can then focus on reducing bias.

Now imagine that human level performance were 7.5%. In this case, we may be doing just fine in the training set and we might want to focus on reducing the variance and decrease the difference between train and dev set.

You should think of human-level error as a proxy (or approximate) for Bayes optimal error.

The difference between the human error and the train error is called avoidable bias. While the difference between the training error and the dev error is still variance.
