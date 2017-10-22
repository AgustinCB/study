# Using an SVM

We will see now what to do to run or use an SVM.

Usually you won't write SVM software yourself, you would use a package that's already implemented to get your parameter \theta. The most common are liblinear and libsvm.

You usually need to specify the choice of parameter C and kernel (similarity) function to use. If no kernel is chosen, then it's called a linear kernel and just uses \theta'x >= 0 to decide if y=1. This is useful if n is large and m is small and you don't want to overfit.

If you chose a Gaussian kernel, then you also need to choose \sigma^2. This is useful when n is small and/or m large.

Depending on what SVM package you use, you may need to implement the kernel function and the package will create the features from here.

If you have features in different scales, it's important to regularise them to avoid the distance to be dominated by the large ranged features.

For other choices of kernel, we need to choose a similarity function that is a valid kernel: I.e. it satisfies the technical condition called "Mercer's theorem" to make sure SVM package's optimizations run correctly and do not diverge. They use to have a lot of smart numerical tricks to optimize the process and to do that they need this theorem.

Some other kernels:

- Polynomial kernel, for example `(x'l)^2`.
- String kernel.
- Chi-square kernel.
- histogram intersection kernel.

## Multi-class classification

Many packages have already built in multi-class classification functionality.

Otherwise, use one-vs-all method (train K SVMs, one to distinguish each of the classes from the rest and then pick the class with the largest \theta[i]'x).

## Logistic regression vs SVM

Suppose that you have n features and m training examples.

If n is large relative to m (for example on a text classification problem with a limited number of traininig examples), you'd usually use logistic regression or an SVM with linear kernel to avoid overfitting.

If n is small and m is intermidiate, you'd use SVM with Gaussian kernel.

If n is small and m is large, create or add more features and then use logistic regression or SVM without a kernel, because SVM would take too long to run.

In this context, logistic regression and SVM without a kernel are usually very similar and perform very similarly.

In all this regimes neural network will likely work well, but may be slower to train. Although in neural networks local minimums are a problem and with Gaussian kernel, for example, that is not.
