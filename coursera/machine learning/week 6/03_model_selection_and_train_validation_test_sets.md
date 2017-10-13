# Model selection and train/validation/test sets

Suppose you want to decide what degree polynomio to use or what features to include in your learning algorithm. Or suppose you'd like to choose the regularization parameter for a learning algorithm. How to do that?

These are called model selection problems and in the next session we will cover not just how to split the data into training and test sets, but into traning, validation and test sets.

## Overfitting example.

We have already seen the overfitting problem and what it does to our predictions. We have also coverted why a low error rate in the training set isn't a good measure on how well we will perform on new examples. A good way to have a measure about how to perform on new samples is to use a test set.

Now let's considere the model selection problem and try to chose what degree of polynomio you want to pick. You want to chose a degree between 1-10 and get an estimate on how well your new model will work on new examples. One thing you could do is perform the whole process on each candidate and then chose the one with the lowest test error.

How do we measure how well the model generalize? We have our test error rate, but that isn't a good pick for this problem. The reason is that we have done a regression over a new feature (the degree of the polynomio) and the test set and is likely that we have fit that case. So the error rate in the test will likely be too optimistic.

To solve this problem, what we do is to create a new set. That way we will have the training set, the cross validation set (cv) and our usual test set. A typical ratio would be 60-20-20%. The cross validation error would work the same as the test error but measured in the cv set.

So what we will do is: For each possible model, we minimize over the training set to get a parameter vector. Instead of test them in the test set, we will test them on the cross validation set and pick the hypothesis with the lowest cross validation error.

Finally, to estimate the generalization error, we will use the error of the test set.
