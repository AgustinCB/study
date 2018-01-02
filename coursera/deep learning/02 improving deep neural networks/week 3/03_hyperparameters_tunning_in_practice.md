# Hyperparameters tuning in practice: Pandas vs. Caviar

In this lesson we will cover how to organize hyperparameters search process.

The best setting of your hyperparameters can get stale, so it's important to re-evaluate them periodically.

There's two ways to manage hyperparameters:

1. Babysit one model: For example, on day 0, you initialize your hyperpameters and start training. Then on day 1, you modify it slightly and see in day 2 how it performed and decide modify it again. This kind of model is used when you don't have a lot of computational power to do multiple models at the same time. This can be referred as the "Panda approach."

2. Training many models in parallel: You would start multiple models at the same time and that would allow you to try multiple parameter settings and make a decision at the end. This can be referred as the "caviar strategy."

The way to choose between this two approaches depends on how many computational power you have.
