# Using word embeddings

In this lesson we will see how to take featurised representation and plug them in a RNN.

Suppose a named entity recognition example. New words in would upset your RNN if you use one-shot, but they would be able to understand the idea of a word if you use featurised representation.

This allows you to carry out transfer learning: You can figure out that the cathegory of a word from the training data and use it as other similar words.

To do so:

1. Learn word embeddings from large text corpus (1-100B words).
2. Transfer embedding to new task with smaller training set (say 100k words).
3. Optional: Continue to finetune the word embedding with new data.

This works if you have a lot of data in step one and small amounts in step two (as we saw before).

Some of this ideas work well with face encoding: Their vector are conceptually similar to word embeddings.
