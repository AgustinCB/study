# Transfer learning

In this lesson we will see how to use weights that someone else already used for a similar application and using them as a base (as the initial values of the weights).

Let's say that you're building a cat detector to detect your own cats. So you have three cases: Tigger, Misty and neither.

You probably don't have a lot of pictures of them, so the dataset will be small. 

You should go online and look for implementations of this dataset. You should also download the weights.

What you can do then is to create your own softmax unit for this three cathegories. Then you'd freeze all the previous layers and focus only in the last layer.

Becuase those are frozen, you could precompute the features of the function that grabs an input and spits the output of layer `L`, save it to disk and use that as the input of softmax classifier.

If you had a larger training set (maybe you have a ton of pictures of your cats), one things you can do is freeze only a subset of the layers and train some of the last ones using the original weights as initialization values. Or you could even throw them and use new layers all together.

If you have a LOT of data, you could use the whole thing just as initilization and just train the whole thing.
