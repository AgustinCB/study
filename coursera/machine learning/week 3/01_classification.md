# Classification

A classification problem is a machine learning problem in which the variable `y` that we try to predict has a discrete value. We'll use an algorithm called logistic regression.

Examples of classification problems:

- Email: Spam / Not spam
- Online transactions: Fraudulent?
- Tumor: Malignant / Bening

In all of this problem, `y` can take two values: "Positive class" (1) and "Negative class" (0). The assignment of the two classes is arbitrary and doesn't really matter. However, there's the idea that the 0 class implies the absense of something.

When `y` has more than two values, it's called binary. When it has more, multiclass.

## Linea regression

Suppose that we have to see if a tumor is malignant or tumor based on the size. 

One thing we can try, is to use the technique we already know and draw a line using our hypothesis. We then threshold the output at 0.5 and if it's greater we predict one and if it's less, it's zero.

This technique won't handle the case of adding new samples way in the extremes of the dataset, it'll move the the threshold to that side even if it shouldn't. It's often not a good idea to use linear regression

Another problem is that `h` can ouput values outside of the expected range, `[0,1]`.

Because of all this, we will use the algotithm Logistic regression, which ensures that the output is in the range.
