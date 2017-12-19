# Train/dev/test sets

In this lessons we will learn practical ideas on how to make sure that machine learning solutions work correctly.

Setting up train, dev and test sets is important to make sure that the network works as expected. They help you identify the correct values for hyperparameters.

Traditionally, you will divide your data in three sets: Training set, dev (or cross validation) set and then the test set.

You will use the first one to train different theories, then pick the one that performs the best in the dev set and see the "real" accuracy with the test set. Usually, you will split it in 60%, 20% and 20%.

In the modern era, with a lot of data, dev and test set usually take smaller percentages. For example, it would make sense to have 98%, 1% and 1% distribution.

It's also important to make sure that the dev and test sets come from the same distribution as the training set.

It may also be ok not having a test set. If you don't need an unbiased estimate, then it may make sense not to have them.
