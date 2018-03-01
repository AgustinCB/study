# Properties of word embeddings

In this lesson we will see how we can use word embedding capability to detect analogies to understand the properties of word embeddings.

Let's say that we have a four dimensional vectors to represent word. And that those dimensions represent gender, royal, age and whether it's food or not.

Suppose now that you have a vector for the word man, another one for the word woman, another for the word king and another one ofr the word queen. And the phrase "Man is to woman as king is to " and you're looking for the next word.

One way to do so, is to compute the difference between the vector of the word "man" and the vector of the word "woman" and try to find to the word whose difference with the word "king" is more similar.

This was introduced in Mikolov et al., 2013.

In general, the word embeddings are a point in an F-dimensional space (where F is the number of features in the representation). Then if the relationship between two words is similar to the relationship of other two words, then the vectors formed by each pair are similar. In other words, given a relationship of two words and an extra word and looking for the forth word whose relationship is similar to the one given as input, we want to calculate:

```
max(w, sim(e[w], e[word1] - e[word3] + e[word2]))
```

Where `word1` would be "man", `word2` "woman" and `word3` "king" in the previous example.

The most commonly used similarity function is:

```
sim(u, v) = (u' v) / (||u||^2 ||v||^2)
```

This is the cosine of the angle between each vector.

We could also use the square distance, although that would be a measure of disimilarity.

Something interesting about this is that they can learn a lot of analogies between words.
