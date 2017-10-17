# Error analysis

In the last lesson we covered how there're different ideas about how to design a learning algorithm. In this lesson, we will cover error analysis, a systematic way to decide which one to implement.

Recommended approach:

- Start with a simple algorithm that you can implement quickly. Implement it and test it on your cross-validation data.
- Plot learning curves to decide if more data, more features or others are likely to help.
- Error analysis: Manually examine the examples (in cross validation set) that your algorithm made errors on. See if you spot any systematic trend in what type of examples it is making errors on.

## Error analysis for the spam classifier

Let's say that we have 500 examples in cross validation set and our algorithm misclassifies 100 of them. We'd manually examine the 100 errors and categorize them based on:

1) What type of email it is. Count each one in each category and see where it does worst to see if you can come up with a better way to handle that case.
2) What cues (features) you think would have helped the algorithm classify them correctly. For example, deliberate misspelling, unusual email routing or unusual punctuation. Again, we'd count them and see what's more valuable to work on.

This process is called error analysis.

This also explains why it's better to often start by a quick and dirty implementation of an algorithm.

## The importance of numerical evaluation

This is very important. We need a way to evaluate our algorithm that tells us how well our algorithm is doing. For example, we are trying to decide whether to treat discount, discounts, discounting and discounted as the same word (this is called steamming and uses a porter steammer software). Using this kind of features may hurt or may help. Or both!

To decide if it's worth to do it or not, error analysis may not be helpful for deciding if this is likely to improve the performance, often the best way to figure it out is to just try it and see if it works. We will run our algorithm with and without stemming and compare the cross validation error to decide what to use.

Having a single raw number to evaluate new ideas will prevent from having to examine examples one by one.
