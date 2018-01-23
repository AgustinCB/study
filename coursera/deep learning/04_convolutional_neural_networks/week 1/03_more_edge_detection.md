# More edge detection

In this lesson, we will see other types of edges and how to have an algorithm learn an edge detector.

Previously we had a filter that was light on the left and dark on the right. But what happens if the colors of the image are flipped? If you use the same edge detector system you'd end up with negative numbers in the column of the middle. If you don't care, you can always take absolute values in the result matrix, but if you do, you'd need a different filter. Fliping the filter horizontally would do the trick.

One the same note, `[[1 0 -1] [1 0 -1] [1 0 -1]]` is an horizontal filter detector. Another interesting option is the sobel filter: `[[1 2 1] [0 0 0] [-1 -2 -1]]`, which puts more weight in the middle.

Something that is often used is treat this nine values as parameters that can be learnt by using backprop and that fits the data better.
