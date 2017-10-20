# Kernels I

We will start adapting SVM to do non-linear classifiers. We will kernels for this.

If we have a training set that needs a complex function to create a good decision boundary, we will need to start using polymonios to express our functions. We will need to do something like this:

```
h[\theta](x) = 1 if \theta[0] + \theta[1] f[1] + ... >=0
               0 otherwise
```

Where `f` is a vector of our polynomial features, for example:

```
f = [x[1], x[2], x[1]x[2], x[1]^2, x[2]^2]
```

However, is there a different/ better choice of features than this high order features? They can be very computationally expensive.

## How to define new features

For this example, we will define three new features depending on proximity to landmarks: `l[1], l[2], l[3]` from a dataset with two input units.

We will manually pick these three points and we will say:

Given an example x, we will define 

```
f[1] = similarity(x, l[1])
f[2] = similarity(x, l[2])
f[3] = similarity(x, l[3])
similarity(x, y) = k(x,y) = exp(-(||x-y||^2)/2\sigma^2)
```

Similarity, here, is called a Gaussian Kernel (similarity functions are called kernels).

## Kernels and similarity

So we have:

```
f[1] = similarity(x, l[1]) = exp(-(||x-l[1]||^2)/2\sigma^2)
```

If x is close to l[1], then the euclidian distance will be close to zero and the feature will be close to one.

If x is far from l[1], then we will have e exponent a large negative number, which will be close to zero.

So f express how close x is to the landmark, with one meaning is the landmark and a value close to zero meaning it's way far away.

\sigma^2 express how rapidly we fall to zero as x moves away from the landmark. The smaller it's, the "faster" we decrease to zero.

Given a training sample x, we will compute this new features f[i] and say that we predict 1 if \theta'f >= 0.

Let's that we have a training example close to l[1] but far from the other two. In that case, we would be able to simply the formula to `\theta[0] + \theta[1] >= 0`.

If we have a point that is far from all the points, we will simplify it to `\theta[0] >= 0`.

This way, we will create a decision boundary around the points that correspond with values of \theta that will gives something bigger than zero. With this way, we can learn a very complex non-linear function.
