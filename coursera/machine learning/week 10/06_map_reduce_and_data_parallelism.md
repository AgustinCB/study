# Map reduce and data parallelism

In this lesson we will explain how to run the algorithms that we covered so far in multiple machines using something called Map-Reduce to help them scale even further than is possible using stochastic gradient descent.

## Map-Reduce

Let's start again with batch gradient descent over 400 examples.

Map Reduce (created by Jeffrey Dean and Sanjay Ghemawat) does the following: It splits the training set into diffent subsets (let's say four, in this example). Then each machine will compute the summation of one subset.

After all that, we send the results to a centralized master center, which will update \theta using the sum of the results.

This is exactly equivalent to batch gradient descent, but dividing the work load between more machines.

To know if you can use map-reduce on your algorithm, you have to ask yourself if it possible to express it as computing sums of functions over the training set.

This doesn't only work with multiple computers, but also with computers with multiple cores.
