# Basic models

In this lesson we will see sequence of sequence models, which are useful form achine translation and speach recognition.

Let's say that we want to to input a phrase in French and translate it to English. The ideas presented are from Sutskever et al., 2014 and Cho et al., 2014.

First lets have a network called an encoder, feeding one word at the time and then outputing a vector that represents the phrase. Then we input that vector to another RNN and output the words one at a time, this is called the decoder.

Something similar to this words for image captioning, i.e. inputing an image and outputing a phrase. We could run the image through a Alexnet arquitechture minus the final softmax. The last vector would be the encoding of the image.

We would then take that and feed it to an RNN whose job it is to generate the caption, one word at a time.
