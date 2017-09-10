# Multiclass classification

In this lesson we will cover how to do multiclass classification through the algorithm one-vs-all.

An example would be an algorithm to tag your email. And you have, for example: work, friends, family and hobby as possible tags.

We would then assign a number to each class, having y in {1,2,3,4}.

Generally, we can say that y can take an small number of more than two values.

We will use a similar idea than the one we used for binary classification: Supposing that we have `n` possible classes, we will convert our dataset in `n` sepparate binary problems.

Each possible value of `n` will be mapped into a binary clsasification problem where y can either be in the current class or not.

After modeling each one of these problems, we will have a set of `n` hypothesis functions, one for each class like this:

```
hi(x) = P(y = i| x;\theta) where y = {1,..,n}
```

To make a prediction, we pass the input to all the classifiers and we pick the biggest one (the one with more probabilities).
