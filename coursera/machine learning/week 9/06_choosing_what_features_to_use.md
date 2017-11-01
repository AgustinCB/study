# Choosing what features to use

When applying anomaly detection, choosing what features to use has a huge effect on the result. We will cover now how to design and select them.

One of the things we did was model the features using our Gaussian distribution `p(x[i]; \mu[i], \sigma[i]^2)`. Plotting the data will give you a sense if the feature looks Gaussian or not. If it does, it's good to add. If it doesn't, we might try to transform the data (for example, to get the logarithm or some root) to get a Gaussian distribution.

Features without Gaussian distribution may work well enough, though, although it'd perform better if they're.

## Error analysis for anomaly detection

We want `p(x)` to be large for normal examples x and small for anormalous examples x.

A common problem is that if `p(x)` is comparable (say both large) for normal and anomalous examples. In this case, the algorithm may fail to flag it.

If we have that example, we may be inspired on what feature to add or modify to be able to catch this case. I.e., what was unusual about that example and how would you model it?

## Choosing features

We'd usually choose features that might take on unusually large or small values in the event of an anomaly. For example, if we're monitoring computers in a data center and we had CPU load and network traffic, we might want to catch a case in which the CPU is very busy because of a weird condition but the traffic didn't increase. We could use then a feature defined as the first one divided by the other. This way you can create features to detect anomalies of unusual combinations of their values.
