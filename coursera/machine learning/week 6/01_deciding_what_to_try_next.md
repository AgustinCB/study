# Deciding what to try next

In this lesson, we will to cover how to chose what's the best algorithm to solve a problem with machine algorithm and how to decide what to try next.

Suppose you have implemented regularized linear regression to predict housing. However, when you test your hypothesis on a new set of houses, you find that it makes unacceptably large errors in its prediction. What should you try next? One may try:

- Get more training. Although this is good, sometimes getting more training data doesn't actually help.
- Try smaller set of features. For example, you may want to manually select the features that make more sense out of a big list.
- Try getting additional features. Maybe our current set of features isn't enough and we want more variables. This can be a huge project and it'd be useful to know in advance if it'd work.
- Try adding polynomial features.
- Try decreasing \lambda.
- Try increasing \lambda.

Many people will randomly pick one of the options and go with that and spend a lot of time working on it with no result.

There's a simple technique to roll out many of these options and we will cover that next. They
're called machine learning diagnostic, a test that you can run to gain insight what is or isn't working with a learning algorithm and gain guidance as how best to improve its performance. It can take time to implement, but is a very useful thing to do before trying to finetune your algorithm.
