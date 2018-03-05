# Negative sampling

In this lesson we will see how to do something similar to npl and word embeddings presented before, but with a more efficient computation. It was presented in Mikolov et. al., 2013.

We will have now a different problem:

We will have a context and a word and an outcome being a class, usually one and zero. One means that the two picks are associated and zero is not.

We will take a context and a word that is related and mark it as one. And then we will keep the context and randomly pick K new words and mark them as zero.

Then we will create a supervised algorithm that will input a pair of words and output whether they're associated or not.

K is usually 5-20 for small datasets and 2-5 for larger datasets.

We had this model before: `softmax(t, c) = p(t|c) = e^(\Theta[t][T] e[c]) / sum(1, 10k, j, e^(\Theta[j][T] e[c]))`.

Now we will have:

```
P(y=1|c,t) = sigmoid(\Theta[T][t]e[c])
```

If we have K examples, we can think of this as having a K-to-1 probability to get an association. It's like having 10k binary logistic regression, but instead of training all of them, we train K+1 examples.

How do you sample the negative samples? One thing you could do is sample according to the emperical frequency of the words, but that will give you a high representation of pronoms or prepositions. In practice, this is better: `P(w[i]) = f(w[i])^(3/4) / sum(1, 10k, f(w[k])^(3/4))`. Where `f(w[i])` is the frequency of `w[i]`.
