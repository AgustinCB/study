# Attention model intuition

In this lesson we will see a modification of the encoder-decoder algorithm called attention model.

Imagine that we have a very long sentence to translate. We would first use an encoder network to encode it and then a different one to generate the English one by using the vector representation.

However, human translator wouldn't do that: They would read one sentence, translate it and then repeat.

As with humans, machine translation also struggles with translation of long sentences in one go. With attention model, we work one part of the sentence at the time. It was defined at Behdenau et. al., 2014.

We will break the sentence into parts and use a bidirectional rnn to generate a vector of features for every part.

To generate the translation, we will have a model that uses something called attention weights.

Those attention weights will say how much importance should we give to a part `y` when processing the part `x`.

During the generation of translation of each part, we would have a different set of weights.
