# Recommender systems: Problem formulation

In this lesson we will talk about recommender systems. They're an important application of machine learning and one of the most widely used in the industry. They're also interesting because you can make them learn what features to use instead of choosing them.

## Example

Let's suppose that you want to predict movie ratings, using a zero to five stars rating system.

Let's say that we have a set of movies with the rating received by different users.

We will use n[u] to denote the number of users and n[m] for the number of movies. We say that r(i, j) = 1 if a user j has rated movie i. y(i, j) will give you the rating given by user j to movie i, defined only if r(i, j) = 1.

Given all this, we want to look our data and predict what would be the missing rating for each user.
