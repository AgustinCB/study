# Transfer learning

Sometimes you can take knowledge that a neural network took from one task to make another task. This is called transfer learning.

Let's say that you trained a neural network on image recognition (for example, recognice dogs or cats) and we want to use it radiology diagnosis.

We will change the last layer (the output layer) for one where it does diagnosis from radiology. You will set the parameters of that layers randomly and retrain the network using the new source. You may just train the last layer (keeping the other ones) or retrain all. Depending on whether you have small or big amounts of data. The first one is called pre-training weights and the second one is called fine-tuning weights.

Instead of just replacing the last layer with another layer, you can also replace it with more layers.

Transfer learning makes sense when both tasks have the same input; when you have a lot of data for one task, but small for another one that is in the same domain and when you suspect that low level features for the first task will be the same as the ones for the second task.
