# Random initialization

In this lesson we will explain why it's important to initialize your weights randomly.

Suppose that we initialize our weights to zero. What happens?

```
W[1] = [[0 0] [0 0]]
b[1] = [0 0]'
```

That would mean that both `a[1][1]` and `a[1][2]` would be equal. Similarly, `dz[1][1]` and `dz[1][2]` would be equal. It's possible to show then that `dw = [[u v] [u v]]`, where every row takes on the same value in all the next W[i] matrixes. This argument works for arbitrarily large hidden layers.

The solution to this would be to initialize your parameters randomly. In python, you'd do: `W[i] = np.random.randn((2,2)) .* 0.01`.

Note that this problem doesn't stand for b, you can just initialize it for zero.

The constant 0.01 comes from the fact that we usually want to initialize the weights to small values so that our activation value won't go to one of the limits of the sigmoid function and therefore to the parts where the slope is close to zero and therefore gradient descent is slow.

Sometimes there are better constants than 0.01. For very deep neural networks, you'd want to think a little bit more about how to pick it, although it'd still be a very small number.
