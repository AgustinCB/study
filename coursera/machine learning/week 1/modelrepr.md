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
