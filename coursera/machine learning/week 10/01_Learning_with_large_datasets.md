# Learning with large datasets

In this lesson we will talk about large scale machine learning.

In the recent history of machine learning, we found that one of the reasons why machine learning improved is because we have now massive datasets.

One of the best ways to get a high performance machine learning algorithm is to get a low bias algorithm and train it in a lot of data. For example, in the case of a classifier for confusable words. This lead to the saying that is not who has the best algorithm who matters, but who has the best data.

Let's say that you have about one hundred million examples in your dataset and you want to train a gradient descent in it. This requires to handle a summation of a hundred million terms for only one step in the gradient descent!

Before seeing how to do that, we should ask ourselves on why not using just one thousand examples and if that would perform just as well. To check this we will plot the learning curve and we should see much our learning objective and cross validation objective behaves and if it's high bias (in which case more samples won't help).
