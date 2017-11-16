# Sliding windows

In this lesson, we will explain how the individual components of the OCR works and how what an sliding windows classifiers is and how it works.

## Text detection

The first step is to detect the text in the image.

You set first the size of the subimages you will test. You build then a supervised learning classifier that will detect whether there's text (a single character) or not in the image.

Then, on a new input, we grab all the subimages of that size and run it through the network. Then, we make the subimages slightly bigger, but with similar ratio, and make the same thing. This is called sliding windows. The step_size or stride is the name of the parameter that determines the lenght of the step to take.

This will give you a map of the characters in the picture, but not locations of text by itself. So you do a second step: We take the output of the classifier and apply what is called an expansion operation. We create an image from that output of black (no character) and white (character) and expand the white regions. Mathematically, for every pixel if it's close to a white pixel, then it will be white too.

This will give you almost rectangular regions in white. If we ignore the ones that are taller than wider and we draw boundary rectangulars in the other ones, then we will have all the text in the images!

We can then cut out this regions and use later stages in the pipeline to try to read this text.

## Character segmentation

The second part of our pipeline is giving an image of only text, we want multiple subimages for every character. What we can use is another supervised classifier that will receive an image and decide whether there's an split on a character on it or not. After training it, we use a sliding window again (although this time it will be in one straight line) over the new inputs and we will get all the locations of the characters in the image.

## Character classification

This is the obvious step: Given the image of a character, classify it to it's alphabat representation.
