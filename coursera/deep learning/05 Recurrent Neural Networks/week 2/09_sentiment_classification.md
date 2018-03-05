# Sentiment classification

In this lesson we will see sentiment classification, a way to look at a sentence and identify the sentiment behind.

One example would be given a sentence about a restaurant, give a rating that would match that restaurant.

A simple classifier would be to look for the words in the dictionary, look up the one-hot vector, multiply by the embedding matrix to extract the embeddings vectors and then take those vectors and sum or average them together and pass that to a softmax classifier that would return a value from 1 to 5, the rating.

This would work decently well. It has one problem, for example, it ignores orden ording. The phrase: "COmpletely lacking in good taste, good service, and good ambience," would return a high scoring because of the good word instead of the low rating it should get.

Another thing we can do is to use RRN: In which we process word by word in different steps till the last one, where we have a softmax following. This is an example of a many-to-one architecture.
