# Satisficing and optimizing metrics

How to combine all the things you care about in one metric? That's often difficult. Instead, you can use satisficing and optimizing metrics.

Let's say that you decided you care about the accuracy about your cat classifier and also about the running time.

One thing you could do is to combine both into one metric `cost= accuracy - 0.5 * running_time`. That may feel artificial though.

Instead, you can create a classifier that maximize the accuracy but subject to a running time lower than a value. The first condition is the optimizing metric and the second one the satisficing metric.
