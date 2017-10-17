# Trading of precision and recall

In the last lesson we covered precision and recall as a different evaluation metric. We often have to trade to decide trade off between each other. We will cover now how to do so effectively.

```
precision = true positives / # predicted positive
recall = true positives / # actual positive
```

Let's continue with the cancer classification problem where `y=1` means cancer.

We will have logistic regression to predict and say that we predict one if `h[\theta](x) >= 0.5` and zero otherwise. This algorithm will give us some value for precision and some value for recall.

Suppose that we want to predict `y=1` (cancer) only if they are very confident. One way to do this is to modify our algorithm to predict one only if our hypothesis is greater or equal of 0.7. With this we are predicting cancer only when we are more confident. We will end up with a classifier with higher precision (the difference between true positives and predicted positive is reduced), but lower recall (because the difference of true positive and actual positive will likely increase).

Suppose that we want to avoid missing too many cases of cancer (avoid false negatives). We don't want to fail a patience to tell them that they have cancer, that could be very bad because they won't get treatment!

To do so, we will modify the algorithm to reduce the threshold and say that we predict cancer if our hypothesis gave us 0.3. So, if there's mor ethan a 30% of chances of having cancer, we will tell them that they have. In this case, we will have a higher recaller, but lower precision.

More generally, we can say that we can set the threshold to decide the tradeoff between precision and recall depending of what's what we prefer to be higher.

## How to chose the threshold automatically and how do we compare them

Suppose that we have three different algorithm, where each one is the same but with different values for the threshold. How do we decide which algorithm is the best?

One of the things we can do is decide a single raw number to measure how well our algoirthm is doing. Because we have two numbers now, we can't do that and is difficult to automate this process. This can slow down the decision making process to decide what is better in contrast to having a single raw number to measure.

How can we get this number? One natural thing to do is the average. This isn't great because it work well with skewed classes, where the one of the numbers will be way higher than the other one. So we shouldn't use the average.

In contrast, there's a different way called F[1] score: `2 * (PR / (P+R))`. This gives the lower value in a higher weight in the average and solves the problem mentioned before. It penalizes cases where one is huge and the other is very small. This a good value to use to make decisions. We can now use our cross validation set to decide which algorithm is better.
