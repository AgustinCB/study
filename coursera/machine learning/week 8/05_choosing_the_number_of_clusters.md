# Choosing the number of clusters

In this lesson we will cover how to choose the number of cluster to run the algorithm. There isn't a great way other than choosing it manually by looking at visualization and representations.

## What is the right value of K?

It's actually often ambiguous to decide what's the right answer and that's part of the problem with unsupervised algorithm and the reason why it's hard to make an automated way to solve thi problem.

## Elbow method

The elbow method runs K-means with different number of clusters and we plot that with respect to the result of the cost function. There'd be a clear elbow where the distortion stops going down rapidly and that's the number of K clusters that you should try.

It's a reasonable way to decide. That's because fairly often, the curve is ambiguous and it isn't that easy to pick the number that makes the difference.

So it's worth a shot, but it's unlikely to work in most common cases.

## Alternative method

Sometimes, you're running K-means to get clusters to use for some later/downstream purpose. We'd often pick K, then, based on a metric for how well it performs for that later purpose.

Coming back to the t-shirt example, we may choose K=3 to have small, medium and large or k=5 to have extra small, small, medium, large and extra large.

This way you based your decision based on business decisions and what will be more profitable downstream.
