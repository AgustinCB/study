# Data for machine learning

In this lesson we will cover the issue of how much data to train on. It's ocmmon to try to go out and collect enormous amounts of data, but one should be cautious about that because is only some times that this will actually help.

However, under certain conditions, getting a lot of data can be a very good way to get a high performant learning algorithm. We will see now which conditions are those.

## Desiginig a high accuracy learning system

Years ago (2001), Banko and Brill made an experiment to see the effect of using different learning algorithms versus trying them out on different training set sizes.

They were considering the problem of classifying between confusable words (to, two or too). Trying to see which one is the appropiate one to go into a certain position in an English sentence. They took a few different algorithms that were considered the state of the art at that point:

- Perceptron (logistic regression).
- Winnow.
- Memory-based.
- Naive Bayes.

The details aren't important. It's just picking different classification problems. The result is that most of the algorithms gave a similar performance and increasing the data increased the performance in every case.

Since then, there's been a lot of studies showing the same: What can really improve the performance isn't the algorithm, but giving a ton of data. This has caused the phrase: "In machine learning it isn't who has the best algorithm who wins, it's who has the most data."

When is this true?

## Large data rationale

Assume that in a problem the features have sufficient information to predict the output accurately. For example, in the previous example, our features will be the surrounding words. In this case the features capture enough information to decide confidently where to put the confusable word.

However, consider the problem of predicting the size of a house from only the size. In this case, there're so many other examples to predict (like the city or the number of rooms), that we don't have enough information to predict the price accurately. In this case, more data won't help.

To test in which case we're, we can ask us the following question: Given the input x, can a human expert confidently predict the output y?

In the first example, an expert in English should probably be able to predict that well.

In contrast, a realtor won't be able to predict the price of a house from only the size.

So let's suppose that we have enough information from the features.

Suppose also that we use a learning algorithm with a large number of features (logistic/linear regression with many features or neural networks with many hidden units). In this case we will a low bias algorithm, because they can fit very complex functions.

Chances are that if we run this algorithm in a training set, we will be able to fit it will and the training error will be small.

If we use a very large training set (massive!), in that case because it's even much larger than the number of parameters, then we have an algorithm that is unlikely to overfit. In this case, our training error will be close to the test error.

In short: If the training error is small and the training error and test error are close, then the test error will be small.

Another way to think of this is: We want to have a an algorithm that doesn't overfit nor underfit. By having many parameters, we set a low bias algorithm that is unlikely to underfit and by using a very large training set, we ensure to have a low variance and be unlikely to overfit too.

So we need to have a lot of information in our features and a very complex algorithm to be able to improve our algorithm by getting more data.

In this context, you should ask:

- Can a human expert predict the output only from the features we selected?
- Can we actually get a large training set?
