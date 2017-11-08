# Mini batch gradient descent

In this lesson we will cover another alternative to gradient descent that can go even faster that stochastic gradient descent.

Mini-batch gradient descent we will use b example in each iteration, where b is the size of a mini-batch that usually is between two and ten. It's kind of a middle term between batch gradient descent and stocastic gradient descent.

The basic idea is to do on every iteration b new examples when performing the descent.

The sum over ten examples can be perform on a vectorized solution that can be easily done in parallel.
