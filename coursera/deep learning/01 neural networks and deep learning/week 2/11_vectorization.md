# Vectorization

Vectorization is the odds of getting rid of explicit for loops in your code. When using big datasets, it's important to run them fast. Those for loops, may harm this and vectorization is the solution.

We need to compute:

z = w'x + b, where w and x are R^n vectors.

With a non-vectorized implementation, you'd do:

z = 0
for i in range(n):
  z+=w[i] * x[i]
z+=b

In a vectorized implementation with numpy, you'd do:

z = np.dot(w,x) + b
