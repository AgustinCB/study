# The problem of overfitting

Both linear regression and logistic regression would work well, however, they can run into a problem called overfitting that can cause them to perform very poorly. We will see what that is in this lesson.

Suppose that we want to predict the price of a house as a function of the size of a house.

Suppose that we start with a model of this form: `\theta0 + \theta1 x`, but we realize that it isn't fitting the data very well (underfit or high bias) because passed certain point the shape of the expected output isn't a straight line anymore. I.e. it has a very strong preconception that the price will increase linealy with the size of the house, despite the fact that the data doesn't do so.

Then we try `\theta0 + \theta1 x + \theta1 x^2` that works good enough. It's "just right."

On the other extreme, we have `\theta0 + \theta1 x + \theta3 x^3 + \theta4 x4` and with that we get a function that passed through all our training examples. On one hand it seems to be doing a very good job: It fits all our data. But it isn't consistent: It will go up on down all over the place and that's not an strong model for predicting houses. This is called overfit or "high variance." This means that is almost as we could fit any function and we don't have enough data to constrain to good hypothesis. Thi scomes when you have too many features, fiting all the training set but failing to generlize new examples (predict prices on new examples).

A similar thing can happen when you do Logistic regression.

# Addressing overfitting

To fix it, a good first approach is plotting the function. However that won't work if you have a high amount of features.

There are two main options:

1- Reduce the number of features: Manually select which features to keep. You can also use an algorithm to decide which features to use (called model selection algorithms). This will work well, but has the disadvantage that by removing features we are also throwing away some of the information we have. Maybe all the features are useful to our prediction.

2- We can also use regularization: Keep all the features but reduce magnitude/values of the parameters \thetaj. It works well when we have a lot of features each of which contributes a bit to predicting the outcome.
