# Orthogonalization

One of the challenges of build machine learning systems is that there's a lot of things to tune and modify. One of the skills of the most efficient engineers in the industry is that they know what to modify to get the effect that they want. This is called orthogonalization.

Orthogonalization is when you design a feature to do only one thing so it's easy to tune it to get what you want.

To tune your machine learning algorithm, you need to tune your features to achieve:

- Fit training set well on cost function.
  - Bigger network
  - Adam
- Fit dev set well on cost function.
  - Regularization
  - Bigger training set
- Fit test set well on cost function.
  - Bigger dev set
- Performs well in the real world.
  - Change dev set or cost function.

The features to improve each objective are different and clearly defined.
