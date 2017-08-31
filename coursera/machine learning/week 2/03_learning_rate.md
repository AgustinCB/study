# Learning rate

We will learn now how to choose the learning rate (`a`).

## Making sure gradient descent is working correctly

The objective of gradiente descent is to find the minimum of `J`. Is often a good idea to plot or see the values values of the function as gradient descent runs showing the number of iterations and the current value of `J`. This trick will show you two things:

- Whether the algorithm is working or not (if `J` is consistently decreasing).
- Whether the function has converged or not (if the `J` keeps stopped changing over time).

There's no way to know in advance the number of iterations necessary to make gradient descent converge.

It's possible, though, to have an automatic convergence test that declares convergence if `J` decreases by less than `10^-3` i one iteration. But choosing what step to choose to declar convergence is quite difficult and may vary from case to case.

## Red flags

Red flags of gradient descent not working:

- If `J` is consistently increasing. Usually because `a` is too big. The fix is to use an smaller `a`.
- If `J` decreases for a while and then goes up and repeats the pattern. Similarly, this is caused by `a` being too big.

It can be proved that:

- For a sufficiently small `a`, `J` should decrease on every iteration. However, if `a` is too small, gradient descent can be slow to converge.

It's often common to try different `a` with `x3` between each other and see which one behaves the best. Generally starting at `0.001`.
