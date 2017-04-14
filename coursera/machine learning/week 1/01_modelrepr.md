# Model and cost function

## Model representation

Assuming a problem in which we have the size of a house and we want an estimated price.

![Model representation](http://g.gravizo.com/g?
digraph modelrepr {
  tset [ label="Training Set", shape=box ];
  lalg [ label="Learning Algorithm", shape=box ];
  shou [ label="Size of house", shape=box ];
  hypo [ label="Hypothesis, h", shape=box ];
  estp [ label="Estimated price", shape=box ];
  tset -> lalg;
  lalg -> hypo;
  shou -> hypo;
  hypo -> estp;
}
)

An hypothesis in Machine Learning is a function that maps from the size of a house with its estimated price.

It'll have the following form:

```
h(x) = C1 + C2 * x
```

Being, therefore, `h(x)` a linear function. In some problems it's needed more complex models, but this is a good way to start. This is called univariate linear regression.

## Cost function

In the previous defined hypothesis, `C1` and `C2` are called parameters.  With different values of these, we have different hypothesis. We should define the values for them that fits better the problem. That's the final objective of linear regression.

### Idea

Choose `C1` and `C2` so that `h(x)` is close to `y` for our training examples `(x,y)`.

### Formalise

Minimize over `C1`, `C2` on `1/(2m) * (E(1<=i<=m) (h(x(i)) - y(i))^2)`. Where m is the number of training examples. I.e.: Find the values of `C1` and `C2` so that the sum of the difference between the outputs of the hypothesis and the actual values is minimal. By convention, the const function `J(C1, C2) = 1/(2m) * (E(1<=i<=m) (h(x(i)) - y(i))^2)`, which is also called the squared error cost function. There are other alternatives, but this is usually the best fit for most linear regression problems.

## Working with a simplified version

Suppose `C1 = 0`. Then we have that `h(x) = C2 * x` and `J` now only depends on `C2`.

Suppose now a dataset of:

```
x | y
- - -
0 | 0
1 | 1
2 | 2
3 | 3
```

As we can see, `h(x) = x` and `C2 = 1`. Therefore, `J(C2) = 1/(2*m) * 0 = 0`. Because the domain of `J` is in the positive numbers, `J(1)` is the lowest possible value. At the same time, because `J` is a quadratic function, the plot of the function will have the form of a parabola. Every point in J correspond to a possible function in the dataset group. 

## Working with the full version

Now, let's add `C1` again to the problem. Because now `J` depends on two parameters, instead of a parable, it has the form of a parabloid. To minimise this kind of functions. A good way to visualise this is with contour figures. In them, each line represents the same value over for value `J(C1, C2)` they're mapping. The minimum value, will be in the center.

What we want is a efficient way to minimise the value of `C1` and `C2`.
