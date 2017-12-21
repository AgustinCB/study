# Binary classification

In this lesson we are going to cover some details on how to implement neural networks conveyed on logistic regression.

We will work over the example of a neural network that recognizes cats in pictures, returning 1 if the image is a cat or 0 otherwise. We will have one feature for each pixel intensity value, starting with the red pixels, following with the green ones and finally the blue ones. That way, if the image is a 64x64 image, our total feature dimension (n) will be 64x64x3 (12288).

A single training example will be represented as a pair (x, y), where x is a R^n vector and y is either zero or one. We can index it to represent an specific training example out of the total of m training examples: (x[i], y[i]) would mean the ith training example.

We can also define a matrix X as follows: [x[1] x[2] x[3] ... x[m]]. This matrix will have the dimensions nxm. We can also stack our results in columns, so Y would be [y[1] y[2], ..., y[m]]. The shape of Y would be 1xm.
