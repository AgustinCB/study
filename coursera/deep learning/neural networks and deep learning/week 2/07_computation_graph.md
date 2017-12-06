# Computation Graph

We can say that the computation of a neural network are organized in terms of a forward pass, which we use to compute the output of the neural network followed by a backwards pass (or backpropagation step) which we use to compute gradients (or derivatives). The computation graph explains why is organized this way.

Let's say that we are trying to compute a function `J(a, b, c) = 3(a+bc)`. To do so we have to do three steps: y=bc, v=a+u and j=3v. We can take this three steps and make a graph as follows:

a\
  -----\
b\     v=a+u -> J=3v
 u = bc/
c/

In this little example, we saw is that through a left pass we get the value. And we will see now how a right to left computation can give you the derivative.
