# NLP and word embeddings

In this lesson we will see word2vec algorithm to work with words, as written in Mikolov et. al., 2013.

Rather than having the context to be the last X words before the target, we would randomly pick a word as a context and a randomly picked word (between a range from the context) as a target.

Let's say that we have a dictionary of 10k words. And now we want to learn the model from some context c to some target t.

We could start by representing c as a one hot vector that we multiply to the the E matrix to get the features. we would then feed that `e[c]` into a softmax unit to output y', that has to be similar to t. So: `softmax(t, c) = p(t|c) = e^(\Theta[t][T] e[c]) / sum(1, 10k, j, e^(\Theta[j][T] e[c]))` and `l(y', y) = -sum(1, 10k, i, y[i] log(y'[i]))`.

There're some problems with this:

Particularly, computation speed. A way to solve this is to have a hierarchical softmax. We would use binary search with binary classifier to optimize the process.

How to sample the context c?

To pick t we need to sample the context c within a window. How do we do that without having always very frequent words being selected (pronoms and prepositions)?

In practise, you wouldn't sample the context completely randomly but with a more complex distribution to make sure that you have a fair representation of relevant words.
