# Error metrics for Skewed classes

In the previous lesson we covered error analysis and the importance of having a single raw number evaluation metric to know how well we're doing. There's one particular case in which finding this number. That's case is known as Skewed classes.

## Skewed classes

Consider the problem of cancer classification. We have a logistic regression model `h[\theta](x0)` where `y=1` means cancer and `y=0` otherwise. Find that you got 1% error on test set (99% correct diagnoses).

But only 0.5% of the patients have cancer. In this case, the 1% error isn't impresive anymore.

Consider this algorithm:

```
function predictCancer(x)
  y = 0
  return y
end
```

That will have 0.5% error rate! Even less than our algorithm!

This setting where the ratio of one class is close to one extreme, this is what we call skewed classes. The problem with using classification error as our evaluation error is the followingÑ

Let's say that we have an algorithm with 99.2% accuracy (0.8 error) and we improve it to 99.5% accuracy (0.5% error). Does this improves the algorithm? One of the nice things of having a quick raw metric to measure an algorithm let us decide whether there was an improvement or not. But going from 99.2% accuracy to 99.5% accuracy isn't particularly useful with skewed classe (because of the low number of one class). By just predicting the big class more often, we can bring down the error even more, even if it's not in a meaningful way.

When we face this problems, we want a different evaluation metric.

## Precision/Recall

Let's say that we are evaluating a classifier in a test set. It's a binary classification problem where the output will be one of zero. If we have a sample where the algorithm predicted one and the actual class is zero, we call it a true positive. If our algorithm predicts that the class is zero and the actual class is zero, that's a true negative. If we predicted 1, but the actual class was zero, it's a false positive. Otherwise, is a false negative.

We're going to compute two numbers:

- Precision: the number of true positives divided by the number that we predicted as positive. I.e., of all patients where we predicted y=1, what fraction actually has cancer? High precision would mean that if our algorithm predicted cancer, there's a high chance that that actually happened.

- Recall: It means, of all the patients that actually have cancer, what fraction did we correctly detect as having cancer? I.e.: true positives divided the number of actual positives. You can also say true positives divided by the sum of true positives plus the number of false negatives.

We want to have a high value in this two numbers. This will gives a better sense of how well our algorithm is doing.

If our algorithm never predicted to have cancer, for example, then recall and precision would be zero, because there wouldn't be any true positives. We solved the problem that we had before!

This will prevent our improvents to happen only because we predict one class more often. As a convention, we set the rare class to the value one.
