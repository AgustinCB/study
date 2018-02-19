# Cost function

In this lesson we will a definition of a cost function for neural style transformation: Given a content image C and a style image S, we want to generate an image G.

We need therefore a function `J(G)` that measures how similar is the content of C and G and adds that to the measure of how similar are the styles of S and G, all this multiplied by relative costs \alpha and \beta. All this was defined in Gatys et al., 2015.

The way the algorithm will run is as follows:

1. Initiate G randomly.
2. Use gradient descent to minimize J(G).
