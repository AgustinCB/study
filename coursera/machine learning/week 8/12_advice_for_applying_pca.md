# Advice for applying PCA

PCA can be used to speed up the running time of an algorithm. In this lesson we will cover how to do that.

Let's say we have a supervised learning problem and that our inputs have a very high number of features (for example, if you're doing image recognition).

We are going to first take our labeled dataset and extract the outputs to create an unlabeled dataset.

Then we will apply PCA and this will give us a reduced representation of the unlabeled data.

Now we will add the output labels to the training set, generating a new one with smaller features.

For new examples, you will do the same: First compress with PCA and then pass to the hiphotesys created to get the prediction.

The mapping from x[i] to z[i] should be defined by running PCA only on the training set. This mapping can be applied as to the examples `x[cv][i]` and `x[test][i]` in the the cross validation and test sets.

## Visualization

Another use case for PCA is visualization of the data. In this case, we will pick k to be either two or three so we can plot correctly the data.

## Bad use of PCA: Prevent overfitting

The logic behind this is that the representation `z[i]` of `x[i]` will have fewer features and will be less likely to overfit.

This might work OK, but isn't a good way to address overfitting. Using regularization may work better.

## Bad use of PCA: Design of ML system

It's often decided to use PCA from the beginning without questioning about using it without. Before implementing PCA, first try running whatever you want to do with the original/raw data. Only if that doesn't do what you want (i.e. if the memory/disk requirements aren't met or if the algorithm is too slow), then implement PCA.
