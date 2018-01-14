# Size of dev and test sets

In this lesson we will see how large your tests should be.

Usually, in previous days, we would break our dataset in 70% train set, 30% test set. Or 60% train set, 20% dev set and 20% test set. And that would make sense, specially with datasets that aren't much bigger than 10000 samples.

But less say that you have one millon training examples. In that case it may make sense to use a train set that is 98% of the dataset, 1% for the dev set and 1% for the test set.

For the test set we need to set it to be big enough to give high confidence in the overall performance of your system. You don't need millions of examples. It's rarely mucho more than 10000 samples.

Sometimes, not having a test set may not be a problem.
