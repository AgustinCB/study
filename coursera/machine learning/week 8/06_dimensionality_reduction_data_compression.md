# Dimensionality reduction: Data compression

We will cover now another type of unsupervised learning algorithm called dimensionality reduction. There're several reasons to use it and one if data compression.

## Dimensionality reduction.

Let's say that we're collecting a big dataset with a bunch of features where one if the lenght of something in cm and another one is the lenght of the same thing, but in inches. This is redudant and maybe we can just reduce the data into only one dimension. 

This isn't an strange example, when you have datasets of thousands of features is possible to end up with redundant data. Also, particularly in this case, is often common to have rounding errors in the features that will make a plot of one with the other not look like an straight line.

If we can reduce the data, that reduces the redundance.

Another example is: Suppose you're creating an automatic driving system for pilots. And you have one feature that is the skill of the pilot and another one that is the enjoyment. You want to get the aptitude as the direction where the line formed between those two features go.

You can then create a new feature that's the position of the example in the line. That is, we want a function that passes from a point in R^2 to one in R that projects the original examples into a line. We can then use that real number as a feature to substitute the other two.

This will allow us to reduce space required and speed up the algorithm. We can apply the same trick into a three dimensional data to convert it into a two dimensional data by projecting into a plane instead of a line.
