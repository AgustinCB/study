# Normalizing activations in a network

One of the most important ideas has been batch normalization. Let's see how it works.

When working with a regression, normalizing the inputs may speed up learning.

When using a neural network, you can normalize the input in a similar way. However, you can also normalize the activation values of every layer. This is what batch normalization does (although it normalizes z and not the activation value).

You would implement it like this:


```
Given some intermediate value in the layer l in a neural network z[1], ..., z[m]
  \mu = 1/m sum(1, m, i, z[i])
  \sigma2 = 1/m sum(1, m, i, (z[i] - \mu)^2)
  znorm[i] = (z[i] - \mu) / sqrt(\sigma2 + \epsilon)
  zprim[i] = \gamma znorm[i] + \beta
```

The effect of \gamma and \beta is that it allows you to set the mean and the variance to whatever you want it to be. If \gamma=sqrt(\sigma2 + \epsilon) and \beta=\mu, then you're inverting the process of normalizing.
