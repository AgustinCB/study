# Improving your model performance

In this lesson we will see a set of guidelines on how to improve your algorithm's performance.

We have two fundamental assumptions of supervised learning:

1. You can fit the learning set pretty well (avoidable bias).
2. The training set performance generalizes pretty well to the dev/test set.

Once you have your avoidable bias and your variance you know what to focus on.

If you want to reduce avoidable bias, you'd:

- Train a bigger model.
- Train longer.
- Use a better optimization algorithm.
- Find a better NN architecture or do hyperparameters search.

If you want to reduce variance, you'd:

- Get more data.
- Use regularization (L2, dropout, data augmentation).
- Find a better NN architecture or do hyperparameters search.
