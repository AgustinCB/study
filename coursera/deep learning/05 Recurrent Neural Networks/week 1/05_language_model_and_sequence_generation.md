# Language model and sequence generation

In this lesson we will see how to build a language model using a RNN.

Language model is function that defines the probability of one phrase to be true (to make sense). I.e. if you were to pick up a random phrase, what is the chance that the next phrase you pick is the input sentence?

To build such a model, you need a large corpus (very large set) of text as a training set. After that, you'll tokenize the sentences you receive as an input and convert every word into one-hot vectors using the vocabulary of the corpus. If there's a word that isn't in your vocabulary, you use the Unknown cathegory.

Next, you'll build RNN model:

At time zero, you compute `a[1]` using a zeroed vector. It will calculate the probability of any word in the dictionary using softmax and will output `y'[1]`.

Then the RNN will try to predict the second word using the input y[1] and a[1]. We will then keep going till the last step in which we will introduce all the words but the last one plus the previous activation value and we will try to predict the next word.

To train this neural network, we need to define the cost function: `l(y', y) = -sum(1, l, i, y[i] log(y'[i]))` and the total loss function: `L=sum(1, s, t, l(y'[t], y[t]))`
