# Picking the most likely sentence

In this lesson we will the difference between some of the models we saw.

We can think of machine translation as building a conditional language model. The example that we used for language model enters one word and uses the output as the input for the next block.

In machine translation we first process a sequence of words to get a vector and then starts of a language model with that representation.

That is why itś called a conditional language model. We can think of it as `P(y[1], ..., y[ty] | x[1], ..., x[t])`, i.e. the probability of getting a sequence of words in one language given a sequence of words in another.

So we are finding the most likely translation. We don't really want to sample randomly though, we want to always get the same output, so we need to come up with an algorithm that maximizes the probability.

One option would be greedy search: Pick the most likely first word, then the second word, etc. However what we want is something that maximizes the probability of the sequence and not of every word. I.e. it's not always optimal to pick one word at a time. Also, if you have 10k words in your vocabulary and you are looking for a 10 word sentence, there's 10k^10 possible solutions.
