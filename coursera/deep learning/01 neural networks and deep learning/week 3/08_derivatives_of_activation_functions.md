# Derivatives of activation functions

In this lesson we will cover how to compute the slope of our activation functions.

## Sigmoid activation function

sigmoid(z) = 1/(1+e^-z)

d/(dz) sigmoid(z) = sigmoid'(z) = 1/(1 + e^-z) (1 - 1/(1+e^z)) = sigmoid(z) (1 - sigmoid(z))

So if z is big, we will have that g(z) = 1 and d/(dz)g(z) will be 1 (1-1) which is close to zero, which is correct. If z=-10, g(z) close to 0 and d/(dz)g(z) will be close to zero, which is true too. If z =0, g(z) = 1/2 and d/(dz)g(z) = 1/2 (1-1/2) = 1/4. Which is also true.

The advantage is that if you already have sigmoid(z), getting sigmoid'(z) is trivial.

## Tanh activation function

g(z) = tanh(z) = (e^z - e^-z) / (e^z + e^-z)

g'(z) = d/(dz) g(z) = 1 - tanh(z)^2

So if z=10, tanh(z) will be close to one and g'(z) will be close to zero.
If z=-10, tanh(z) will be close to -1 and g'(z) will be close to zero.
If z=0, tanh(z) = 0 and g'(z) = 1.

All of them true. As with sigmoid, if you already have g(z), g'(z) is trivial.

## ReLU and Leaky ReLU

g(z) = max(0,z)
g'(z) = 0 if z < 0
        1 if z > 0
        undef if z = 0 // Although it works fine on software, because the chances of arriving that point are so small that it doesn't matter

gl(z) = max(0.01z, z)
gl'(z) = 0.01 if z < 0
         1 if z > 0
