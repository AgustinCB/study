# Gradient descent on m examples

In this lesson we will see how to implement gradient descent for m examples.

J(w, b) = 1/m sum(1, m, i, l(y'[i], y[i])) = -1/m sum(1, m, i, (y[i] log(y'[i]) + (1-y[i]) log(1-y'[i])))
J(w, b) = 1/m sum(1, m, i, l(a[i], y))
a[i] = sigmoid(z[i])
z[i] = w't x[i] + b

d/dw[i] J(w,b) = 1/m sum(1, m, i, d/dw[i] l(a[i], y[i]))

The algorithm:

J=0, dw1=0, dw2=0, db=0
for i=i to m
  z[i] = w'x[i] + b
  a[i] = sigmoid(z[i])
  J += -[y[i] log(a[i]) + (1-y[i]) log(1-a[i])]
  dz[i] = a[i] - y[i]
  dw1 += x[i][1] dz[i]
  dw2 += x[i][2] dz[i]
  db += dz[i]

J /= m
dw1 /= m
dw2 /= m
db /= m

w[1] = w[1] - \alpha dw1
w[2] = w[2] - \alpha dw2
b = b - \alpha db

This implementation requires two for loops, which is often less efficient. Using vectorization, one can get rid of this.
