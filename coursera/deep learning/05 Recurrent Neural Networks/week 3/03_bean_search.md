# Beam search

In this lesson we will learn Beam search, a way to maximize a search, used often in machine translation or speech recognition.

Let's use machine translation as an example. The first thing it does is to try to pick the first word of the target language given the input sentence. It has a parameter `B` (bean width), which sets the number of words it should consider at a time. I.e. it will select `B` number of words as the best possible first words for the translation of the sentence. It'll then try to translate with those `B` words as the first one at the same time.

Then it will evaluate the probability of the second word given the input sentence and the first word. So `P(y[2] | x, y[1])` instead of `P(y[1],y[2] | x)`. This for each possible first word and each word in the vocabulary.

So in total, it will consider `B` times the size of the size of the vocabulary. Then it will pick the top three aout of those `B * len(vocabulary)`. It will then repeat this process till arriving to `EOF` in each branch.

When `B=1`, we have greedy search
