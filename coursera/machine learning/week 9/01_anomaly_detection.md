# Anomaly detection

In this lesson we will start speaking about a problem called Anomaly detection, which is a very common type of machine learning problem. Many aspects of it are similar to both supervised and unsupervised algorithm.

To explain it, let's use this example: Imagine that you're a manufactor of Aircraft engines and you're doing some Q&A testing and as part of that testing you measure features of the engine such as heat generated and vibration intensity.

Now you have a dataset of unlabeled data. The next day you have a new engine with new features. And we want to know if there's something weird in the data for that engine and if it's ok to ship it to the customers without further testing.

If our new point is in the middle of the datasets, then is likely that is ok. If it's an outlier, we will say that isn't ok, since it doesn't look at all as anything we have seen before.

More formally, in the anomaly detection we are giving a non-anomalous dataset and given a new entry we want to know if it's anomalous or not. I.e. we want a model that given the features we decided that will give us the probability of x so that:

```
p(x[test]) < \epsilon then there's an anomaly
otherwise everything is ok
```

The probability will be higher as the point is closer to the middle of the dataset.

The most common application is fraud detection, to detect which users are real one.
