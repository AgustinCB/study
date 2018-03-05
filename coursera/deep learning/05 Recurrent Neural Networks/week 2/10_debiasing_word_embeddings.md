# Debiasing word embeddings

In this lesson we will see how to remove bias in word embeddings.

When using bias in this context we mean gender, social economic status or race.

This was explored by Bolukbasi et. al., 2016.

Let's say that we have already learned the word embeddings of our dictionary. Then we are going to:

1. Identify the direction of the bias we want to reduce.
  - If we want to reduce gender bias, for example, we could take the difference of some pairs of definitional words such as "he" and "she" and "male" and "female" and average them. That would be our direction vector.
2. Neutralize: For every word that is not definitional, project to get rid of bias.
3. Equalize pairs. In this case, you want the only difference between definitional words to be the direction vector.

So, how do you decide which words you should neutralize? You can train a classifiar to figure out which words are definitional and which ones aren't and use that to decide.

The equelizable pairs are far smaller and can probably be picked by hand.
