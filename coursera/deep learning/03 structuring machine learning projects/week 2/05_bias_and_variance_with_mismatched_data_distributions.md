# Bias and variance with mismatched data distributions

In this lesson we will see how to analyse bias and variance when your data comes from data distributions.

Let's say we have a cat classifier and that the humans get near a 0% error. Let's say that we have a training error of 1% and a dev error of 10%. If the data of both datasets came from the same distribution, we would know that we have high variance (the algorithm didn't see the data in the dev set).

If they come from different distributions, this is not necessarily true. It may be that the dev set is just harder and the training set is easier.

To know what's happening, we will create something called training-dev set. It will have the same distribution as the training set but not used for training. It's a subset, therefore, from the training set.

You'd run the classifier on the remaining of the training set and compare with both the training-dev and dev set. Let's say that we got:

Training error of 1%.
Training-dev error of 9%.
Dev error 10%.

In this case the difference between the training and the training-dev problem went up a lot. This tells us that there's a variance problem: The algorithm does well on the training, but it doesn't generalize well.

On the other hand, if we get something like htis:

Training error of 1%.
Training-dev error of 1.5%.
Dev error of 10%.

In this case, it's a data mismatch problem because your learning algorithm was trained to do well in data you don't care about.

If you got, finally:

Training error of 10%.
Training-dev error of 11%.
Dev error of 12%.

In this case, because the human error is 0%, you have a high bias problem.

Finally:

Training error of 10%.
Training-dev error of 11%.
Dev error of 20%.

This means that we have a two errors: Avoidable bias problem and a data mismatch problem.

So we have to look at:

- Human level error.
- Training set error.
- Training-dev set error.
- Dev set error.

Using this, you can see what are the problems (if any) of your dataset.

As a last note, if there's a huge gap between the dev and test set it means you overfitted the dev set. In this case, you should be getting more dev data.

Suppose that you have a bigger error in the training set and training-dev set than in the dev error. This is usually because the dev set is easier.

So suppose that you have an application to do rearview mirror speech recognition. And you have this table:

		General speech recognition data		rearview mirror speech data
Human level	4%					6% (measured)
trainted on	7% (train error)			6% (algorithm with trained with data)
Not trained on	10% (train-dev error)			6% (dev/test error)

This explains that the data you trained on is just harder for the algorithm than the data you're interested on.
