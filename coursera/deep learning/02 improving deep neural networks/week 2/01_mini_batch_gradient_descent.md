# Mini-batch gradient descent

One thing that makes deep learning difficult is that training in a big dataset is slow. We will see how to solve this by using mini-batch gradient descent.

Vectorization allows you to efficiently compute on m examples. However, if the dimensions of the matrix is big, then the algorithm is still slow, which is the problem that gradient descent faces. One way to solve this is to split your training set in T mini-batches and then doing:

```
for t = 1,..., T:
  # implement gradient descent over that subset using vectorization
  forward prop on X[T]
  compute cost using X[T] and Y[T]
  backward prop on X[T] and Y[T]
  update the weights
```
