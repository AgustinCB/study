# Features and polynomial regression

Let's take again the example of the price of a house. Suppose we have two features, the frontage (width) and the depth of the house.

The first idea would be:

```
h(x) = C0 + C1 * x1 + C2 * x2
```

However, you don't necessarily need to use the features you're given. For example, you may decide that the important information here is the area and not the frontage or the depth:

```
h(x) = C0 + C1 * (frontage * depth)
```

Sometimes by defining new features, you may get a better model.

## Polynomial regression

Close to this, not always an straight line fits the data in the best way. Often you may decide that a different polynomial model (such as a quadratic or a cubic model).

To do linear regression in this kind of scenarios, we will just add new features. 

As an example, let's use again the size of a house as a feature and try to make good cubic model to predice prices.

```
h(x) = C0 + C1 * x1 + C2 * x2 + C3 * x3
     = C0 + C1 * (size) + C2 * (size)^2 + C3 * (size)^3

x1 = (size)
x2 = (size)^2
x3 = (size)^3
```

Now we can apply the same techniques we saw before over this features and end up with a cubic model for your data.

When doing this, feature scaling becomes very important! As now you have features that grown in exponential speed.
