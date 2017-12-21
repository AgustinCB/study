# More vectorization examples

In this lesson we will look at a few more example around vectorization. In general, whenever possible, avoid explicit for-loops.

If ever you want to compute a vector as the product of a matrix A and a vector v, you'd do:

u = np.zeros(n, 1)
for i in n
  for j in m
    u[i] += A[i][j] * v[i]

The vectorized implementation would be: u = np.dot(A, v)

Say you need to apply the exponential operation on every element of a matrix. 

Instead of:

u = np.zeros((n, 1))
for i in range(n):
  u[i] = math.exp(v[i])

However, you can do the following: u = np.exp(v)
