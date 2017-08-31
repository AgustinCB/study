# Linear regression with multiple features

In the previous uses of linear regression we had a single parameter, `x`, to predict values. For example, we had the size of the house to predict the price. However, it's often more useful to also keep in mind the number of bedrooms, floors or years of the house.

We will start by adding some notation: 

```
n = number of features
xi = features of ith training example (it is n dimentional vector also)
x{i,j} = value of feature j in ith training example
```

## Form of the hypothesis.

With only two features, we had:

```
h(x) = C1 + C2 * x
```

With multiple features will have now the form:

```
h(x) = C0 + C1 * x1 + C2 * x2 + C3 * x3 + ... + Cn * xn
```

For convenience, we will say `x0 = 1`. With that, we can say that:

```
h(x) = sum(1, n, i, Ci * xi)
```

We can also think it as a dimensional vector `X = (x0, ..., xn)` and a dimensional vector `C = (C0, ..., Cn)` and say: 

```
h(x) = transpose(C) * X
```

## Gradient descent

Now gradient descent looks like:

```
Repeat {
  Cj := Cj - a * d/(d*Cj) * J(C)
}
```

Where `C` is the dimensional vector formed by `C0, ..., Cn`.

Let's expand:

```
Repeat {
  Cj := Cj - a * 1/m * sum(1, m, i, (h(xi) - yi) * x{i, j})
}
```

Note that because `x0 = 1`, when `n = 1`, this is still the same as the formula we saw in previous videos for two parameters functions.
