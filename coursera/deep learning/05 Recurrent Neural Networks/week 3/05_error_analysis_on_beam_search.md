# Error analysis on beam search

In this lesson we will see how to do error analysis with beam search.

Let's say that we use beam search to do machine translation and we want to locate the source of a problem.

We have two components: An RNN (with a decoder and an encoder) and the beam search algorithm. It's tempting to increase the beam parameter, but that might not work as expected.

The RRN computes `P(y|x)`. We can use it to compute the P for the result of the algorithm and for the human translation and compare one with each other.

Let's say that `y*` is the human translation and `y'` is the algorithm generated.

If we had `P(y*|x) > P(y'|x)`, then the beam search chose y', but `y*` attains higher probability. So beam search is at fault.

If we had that `P(y*|x) <= P(y'|x)`, `y*` is a better translation than `y'`, but RNN predicted the opposite. In this case, the RNN model is at fault.

You'd do this for a bunch of different phrases. You can then figure out what fraction of errors are due to beam search vs the RNN model.

If beam search is responsible for a bunch errors, then it may make sense to increase `B`.
