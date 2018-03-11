# Bleu score

In this lesson we will cover how to evaluate machine translation systems where there's multiple good answers. This is often done with something called bleu score.

Let's say we have a french to english machine translator. A phrase could have two (or more) perfectly fine translation. Bleu score computes an score to measure how good the translation is.

So long the machine is close to a reference given by humans, it will receive a high score. Bleu means bilingual evaluation understudy.

It was created in Papineni et. al., 2002.

Let's say that we have a very machine translation. One way to measure how good it is is to look at a word in the output and see if they appear in hte reference provided by humans.

If we just repeat one word several times, it'd receive a high precision score. So instead we would use a modified precision measure: We would count how many times a word gets credit based on how many times it appears in the reference as maximum.

Another thing we would try is to use bigrams: Pairs of words. This will help keep the order of words.

We would count how many times each bigram appears in the machine learning algorithm, then we will check how many times it appears in our reference phrases. Then the score would be that number divided by the total number of bigrams.

We could do the same with n-grams.

By convention, Bleu score is defined as `BP * exp(1/4 sum(1, 4, n, pn))` where `pn` is the n-score and `BP=brevity penalty`, which prevents very short translations to high very high bleu scores. In general,

```
BP = 1 if MT_output_length > reference_output_length
     exp(1 - MT_output_length/reference_output_length) otherwise
```
