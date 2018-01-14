# Train/dev/test distributions

In this lesson we will see on how to set your datasets to maximize your teams efficiency.

You try ideas in your training set, you use the dev set to pick the best one and the test set to see how well it would perform.

Suppose that you have a test classifier that works in different regions. You could choose some regions to go to the dev set and another ones to go to the test set, but this is a very bad idea: That means that every set would come from different distributions. It's like setting a target, train in that target and then move the target to test there.

A better idea would be to randomly shuttle the data into the dev and test set.
