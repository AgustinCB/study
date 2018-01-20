# Addressing data mismatch

In this lesson we will see how to address the problem of data mismatch.

You can start by carrying out manual error analysis to try to understand the difference between training and dev/test sets. For example, in the rearview example, you may listen to examples and see what's the difference between each other. When you have insights in this, you can find ways to make the training data more similar to the dev data or collect more data similar to the dev/test sets with the problems that you found earlier.

One of the techniques you can use is to create artificial data. There's one note of caution here: If they're all built from one relatively small source, then it's very possible that your algorithm will overfit to it.
