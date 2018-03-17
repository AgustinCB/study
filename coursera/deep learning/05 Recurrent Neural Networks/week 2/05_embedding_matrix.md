# Embedding matrix

In this lesson we will see how to implement word embedding using an embedding matrix.

Let's say that we have a 10000 word vocabulary. We want to learn a word embedding matrix E of dimension 300x10000. Each column will correspond to the feature vector of the word in that position in the vocabulary. So if we multiple `E*O[i]` where `O[i]` is the one-shot vector of the word in possition `i` in the vocabulary, we get a 300x1 vector that is equal to the feature vector of that word. It selects out the embeddings for the word and we will call it `e[i]`. In practice, use a specialized function to look up an embedding, as multiplication is very expensive.

We will initialize E randomly and then use gradient descent to learn the content of the matrix.
