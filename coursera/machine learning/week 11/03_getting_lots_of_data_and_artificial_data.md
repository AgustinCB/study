# Getting lots of data and artificial data

In this lesson, we will cover what artificial data synthesis is and how to use to get a huge training set.

This can be achieved either by creating data from scratch or by using already existing data to create new one.

## Character regnition

We want to take an image as input and recognise which character is in the middle of it.

One thing we can do is to take characters from a font and paste in a random background. This way you can create a synthetic dataset. It takes job to make it actually look real.

The other main approach is to take examples we currently have and create additional data from it. For example, if we have an image of a letter, we can apply image operations/distortions on it to get more examples.

A warning about this: The disturtion that you introduce should represent a type of noise/distortion that exist in the test case. Usually it doesn't help to add purely random/meaningless noise to your data.

## Discussion on getting more data.

1. Make sure you really have a low bias classifier before expending the effort (for example, plot learning curves). If you don't have it, keep increasing the number of features/number of hidden units in the neural network until you have a low bias classifier.

2. Ask yourself: "How much work would it be to get 10x as much data as we currently have?" If the answer is that not much, then it's worth to go with it. You can use the artificial data methods mentioned here or to collect/label it by yourself. You can also use a "crowd source" to fairly inexpensively label data.
