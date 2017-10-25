# K-means algorithm

We are trying to group data with no label. To do that, the k-means algorithm is the most used one.

Let's say that I want to group a dataset into two costumers. The I'd:

- Randomly initiliaze two random cluster centroids.
- Repeat until converge of position centroids:
  - For each point in the dataset, assign to the centroids that is closer (the square of the norm of the difference between the point and the centroid).
  - Move the centroids to the mean of the points. The mean of group of points is calculated by the average of each element in the vector. If a cluster don't have points assigned, you can either remove it or set it up randomly again.

The first is called assignment step and the second move centroids step.

For this, we take two inputs:

- K (number of clusters).
- Training set (unlabeled).

## K-means for non-separated clusters

Very often K-means is applied to datasets that don't have a well separated dataset. For example, if you want to match to get different t-shirt sizes from the weight and height of people. Even with this cases, k-means will separate the data correctly. This is a good example of market segmentation.
