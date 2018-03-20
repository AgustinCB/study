# A note on python/numpy vectors

The hability of python to allow you to use broadcast operations is both a strength and a weakness. It's strength because is flexible, but also a weakness because it's easy to make some hard to find mistakes. We will share some tips and tricks to eliminate a lot of the strange looking bugs in your code.

One of the less intuitive effects, we can see:

```
import numpy as np
a = np.random.randn(5) # A vector with 5 random gaussian values not a matrix!
assert(a == a.T)
np.dot(a, a.T) # will return a number!!
```

In general, try to avoid this kind of data structures. Using `np.random.randn(5,1)` will return a column matrix instead of a vector.

Vectors have shape (5,). THey don't behave consistently as either a row nor a column vector. In general, try to avoid them by making them matrixes explicitely:

```
a = np.random.randn(5,1) # column vector
a = np.random.randn(1,5) # row vector
```

Another good idea is to assert the shape of the matrix so you make sure that the shape is what you'd expect. If you end up with that kind of array, you can do:

```
a = a.reshape((5,1))
```

Which will convert the vector into a matrix.
