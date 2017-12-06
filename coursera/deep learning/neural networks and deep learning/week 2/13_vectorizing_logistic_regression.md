# Vectorizing logistic regression

In this lesson we will vectorize completely logistic regression. We had so far:

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

Which can be improved to:

J=0, dw=np.zeros((n, 1)), db=0
for i=i to m
  z[i] = w'x[i] + b
  a[i] = sigmoid(z[i])
  J += -[y[i] log(a[i]) + (1-y[i]) log(1-a[i])]
  dz[i] = a[i] - y[i]
  dw += x * dz[i]
  db += dz[i]

J /= m
dw /= m
db /= m
w = w - \alpha * dw
b = b - \alpha db

To remove the last explicit for loop, we need to ocmpute all the intermediate a and z values. To do so, we will define X as our inputs stacked as columns. With that, we can compute X as:

z = w'X + b => z = np.dot(w.T, X) + b

Where b is an m dimensional vector with all values set to the parameter b. For the a values, we would do:

a = sigmoid(z)

Where sigmoid receives capital z and very efficiently returns a vector of as.
