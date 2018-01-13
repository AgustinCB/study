# Single number evaluation metric

When trying different options for building a machine learning system, you'll find that things are easier when you have a single number evaluation metric. Teams often define one at the beginning and use it to measure the performance of their options.

One reasonable way to evaluate the performance of an algorithm is using "precision" (of the example recognized as cats, what percentage actually are cats) and "recall" (of all the images that really are cats what percentage was correctly recognized as cats). There's often a trade off between them both

The problem with using them is that if a classifier does better on Recall, but another does better on precision, you don't know which one is the best. And you want to know how to quickly pick one as the best.

For that, we can define a new metric called F1 score: `2/(1/P + 1/R)` (the harmonic mean of precision and recall).

Having a well defined dev set plus a single number evaluation metric allows you to quickly tell which algorithm is better out of a set and that speeds up iterating.

Let's say that you're build a cat classifier for different geographies and it achieves different errors depending of the geography. Is difficult to know which classifier is superior from the error of the countries. You can use instead the average to choose which one is the best.
