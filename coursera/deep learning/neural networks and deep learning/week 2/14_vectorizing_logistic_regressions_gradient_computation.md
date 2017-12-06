# Vectorizing logistic regression's gradient computation

In this lesson, we will see how to use vectorization to do the gradient computation for logistic regression.

We used to do:

dz[1] = a[1] - y[1] ... dz[m] = a[m] - y[m]

We will define then a vector (1xm) to do so that will use our previously defined a and y vectors:

dz = a - y

Now we can modify our algorithm to be more compact:

z = np.dot(w.T, X) + b
a = sigmoid(z)
dz = a - y
dw = X * dz.T / m
db = np.sum(dz)/m
w = w - \alpha dw
b = b - \alpha db

And now we have a single iteration for the gradient descent without needing to use a for loop.
