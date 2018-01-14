# Understanding human level performance

In this lesson we will look for a definition for "human-level performance."

First we have to keep in mind that we will use "human-level performance" as a proxy for Bayes error.

Let's say that we want a machine learning algorithm that will look at a medical image and see what's wrong there.

We have then:

1. Typical human: 3% error
2. Typical doctor: 1% error
3. Experienced doctor: 0.7% error
4. Team of experienced doctors: 0.5% error

Then what's "human-level error"?

We know that by definition Bayes error would be lower or equals as 0.5%. So we can say that human-level error would be that.

To see why this matter consider that your training error is 5% and your dev error is 6%. Our human error would be 0.5%. The avoidable bias (gap between training error and human error) is bigger than the variance (gap between training error and dev error). In this case is clear that the avoidable bias is a bigger problem than the variance. Now suppose that the training error is 1% and the dev error is 5%. In this case, it doesn't really matter the avoidable bias, no matter which one you choose from the above. Now suppose that your training error is 0.7% and your dev error is 0.8%. In this case is really important that you use 0.5% as the human error, because the avoidable bias would be twice as big as the variance and otherwise you'd have missed this.
