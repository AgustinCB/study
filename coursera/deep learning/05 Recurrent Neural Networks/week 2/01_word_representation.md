# Word representation

In this lesson we will see how to represent words using word embedings.

So far we have been representing words using a vocabulary and one-hot representations.

This has the problem that it doesn't express relationship between words and how closed related they might be. For example, the dsifference between orange and apple is the same as the difference between orange and king.

It'd be nice if we had some kind of featurised representation so that we could compare them between each other. We could pile up hundred of properties and measure them for each word, putting them in one vector from which we can calculate differences.

We can then use word embeding algorithms (such as t-SNE) to put words together depending on features. We call this featurized representation of words embeddings.
