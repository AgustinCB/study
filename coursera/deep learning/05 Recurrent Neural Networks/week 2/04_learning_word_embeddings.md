# Learning word embeddings

In this lesson we will see some concrete algorithms on how to apply word embeddings. The algorithms used today are simpler version of very complex early versions. We will start with the complicated ones and work it out to the simple ones.

Let's say you're building a language model with a neural network. We have a sequence and we want to predict the next word. We will use the ideas in Bengio et. al., 2003 to show how.

We first get a one-shot vector for every word. Then we go to a matrix E with features to get `e[i]`, the feature vector.

We then feed all of those feature vectors to a neural network with a softmax as an output with 10000 outputs (a one-shot representation of the output word).

This algorithm would learn pretty decent word embeddings. In this case the E matrix is a parameter, alongside with `W` and `b`, that gradient descent has to learn.

It's common when doing language modeling to get a "context" when predicting the next word. For example, the four words on the left (and/or the right). It's natural, in genral, to use the last t words as a context.
