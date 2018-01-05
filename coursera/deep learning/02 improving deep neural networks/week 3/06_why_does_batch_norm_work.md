# Why does batch norm work?

We have seen how to normalize your features to have avg 0 and var 1 works, a similar reason is why batch norm works on intermediate units in the neural network.

Let's say that we are training a shallow network and we train it with all images of black cats. When using colorful cats, the network won't be able to perform well.

This is because your data distribution changed. This is called covariate shift.

In a deep network with four networks, let's get the perspective from the perspective of layer 3. It gets some sets of values from the earilier layers and has to do some stuff for the next layer.

The values that it gets (the vector a[2]) can be thought as features and the remaining networks as a shallow neural network. Also from the perspective of layer 3, those values, the input changes all the time. That makes the distribution of the changes to change all the time. What batch norm ensures is that the mean and variance of at vector remains always the same. It reduces the problem of the input values changing. This makes the later layers have less changes to adapt to, speeding up the network in the whole network.

Each mini-batch is scaled by the mean/variance computed on just that mini-batch, this adds some noise to the values z[l] within that minibatch. So similar to dropout, it adds some noise to each hidden layer's activation. This has a slight regularization effect.
