# Cleaning up incorrectly labeled data

In this lesson we will cover how to clean your data for cases in which the label is incorrect.

Let's say that we have a dataset of cats and no-cats with some incorrectly labeled samples.

In the training set, algorithms are quite robust to random errors there. So long as the errors are rasonably random and the dataset is big enough, there's no need to fix them. There's on caveat to this: If the error is systematic (for example, we consistently labeled white dogs as cats), the algorithm will learn wrongly that example.

In your dev/test set, you should add an extra column during error analysis for incorrectly labeled cases. As you check the percentage of error cathegories, you also check the incorrectly labeled examples and if it makes a significant difference to your ability to use your classifier, you go and check it.

To decide if this is the case, you would check:

- Overall dev set error.
- Errors due incorrect labels (percentage of overall error).
- Error due to all other causes (percentage of overall error).

If the percentage of incorrect labels is small compared with the other causes, there's no need to fix it. If not, you should.

Also rememer that the goal of dev set is to help you select between two classifiers, so you have to trust your dev set.

Some guidelines for cases in which you want to fix incorrect dev/test set examples:

- Apply the same process to your dev and test sets to make sure they continue to come from the same distribution.
- Consider examining examples your algorithms got right as well as ones it got wrong or you will end up with biased set.
- Train and dev/test data may now come from slightly different distributions.
