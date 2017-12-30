# Vanishing/exploding gradients

When you're training a very large neural networks, your derivatives (your slopes) can be sometimes very very big or very very small and this makes training difficult. We will see how to significantly reduce this problem by using random initializations.

Let's say you're training a very deep neural network, with L layers, a linear activation function g(z) = z and b[l] = 0, so we can say that: y = mult(L, 1, l, W[l]) * X.

Let's say that each of the weight matrixes is slightly larger that the identity:

```
W[l] = [[1.5 0] [0 1.5]]
y = W[l] [[1.5 0] [[0 1.5]]] ^ (l-1) X
```

This will make the value of y explode. If we replace 1.5 with 0.5, we have the opposite effect, the activation values will decrease exponentially as a function of l. A similar argument can be made about the gradients of the neural network. This makes training very difficult, specially if the gradients were very small. This problem was a huge problem during years in deep learning.
