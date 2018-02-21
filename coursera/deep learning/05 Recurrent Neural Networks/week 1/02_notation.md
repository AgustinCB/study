# Notation

In this lesson we will see what notation we will use to build a sequence model.

Let's suppose that we want a model that given a sentence it tells you all the names in the sentence. We will model the output as a vector with one bit per word and that bit being one if the word is a name.

We will assume the input is a vector of words and we will indexed as such (so w[1] is the first word).

`T[x]` will denote the size of the input and `T[y]` will be the output size of the output.

We often create a a vocabulary, a dictionary with all the words that may expect as input.

After that, you can use one-hot representation to encode each word.

We will call `unknown` words that aren't in our vocabulary.
