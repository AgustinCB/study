# Different types of RNNs

In this lesson we will see how to handle RNNs with different architectures.

We saw, so far, RNNs where Tx and Ty are the same. This is a many-to-many application. That's not always the case.

Now let's suppose a different architecture:

Int a sentiment classification problem, our input would be a text and our output zero or one on whether it's possitive or not. It'd be similar to our previous architecture, with the exception that it won't have an output in every "step," but only at the end. This is a many-to-one architecture.

A One-to-many application would be, for example, music generation. There, our input would be x and after that pretty much a many-to-many using the previous output as the input of the next layer.

There's another example of many-to-many where the input and output length are different. For example, machine translation.

In this case, we would have a set of layers to read the sentence and then another set of layers to output the translation. The first one is called encoder and the second one is called the decoder.

A one-to-one would be an standard neural network.
