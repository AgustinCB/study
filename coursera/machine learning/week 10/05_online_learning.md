# Online learning

In this lesson we will cover a new large scale machine learning setting, called online learning. It's based on the idea of a continous stream of information from which we want to learn from.

Suppose you run a shipping service website where user comes, specifies origin and destination and you offer to ship their package for some price and users sometimes choose to use your shipping service (y=1) and somethimes not (y=0).

Let's say that we want a learning algorithm to decide that price. The features we want to capture are the user, origin, destination and the asking price. We want to learn `p(y=1|x;\theta)` to optimize price.

We could use something like logistic regression.

```
Repeat forever {
  x, y = getUserXAndY()
  updateTheta(x, y) {
    \theta[j] := \theta[j] - \alpha(h[\theta](x) - y) x[j] for j in 0:n
  }
}
```

This is great when you have a big number of users. One good thing about it is that it can adapt to changing user preferences.

Other example may an application that sells mobile phones. We let users search for phone characteristics and we will be returning 10 results for that query.

What we can do is to construct a feature x for each phone and user query (for example, how similar they both are). After that, we'd like to estimate what's probability of a user clicking on a link: `p(y=1|x;\theta)`. This is called 'predicted ctr.' With this, we can predict the 10 phones that they're most likely to click on.
