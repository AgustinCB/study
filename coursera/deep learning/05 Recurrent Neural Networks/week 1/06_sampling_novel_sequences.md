# Sampling novel sequences

In this lesson we will see how to do sample novel sequences.

A sequence model models the chance of any particular sequence of words. What we want to do is to sample from this distribution to get novel sequences of words.

What we want to do is first sample the first word we want to generate. That first step will give us a softmax distribution. We will then sample from the distribution described in the softmax distribution. we use zeros as input for this step.

From there, for next iterations, we pass the previous activation value and the previously selected word. The output of this is again a distribution from which we randomly select one value.

How do you know when the setence ends? You can add a token to mark end of sentence to your vocabulary and finish there.

This particular process may return an unknown token, we could prevent that by forcing the algorithm to reject such outputs.

Another option would be to create a character level RNN. In this case our vocabulary would be the list of possible characters.

Using this has some pros and cons: You don't have to worry about unkwnon tokens, for example. However, you end up with much longer sequences and therefore they will strugle more in taking previous data into account.
