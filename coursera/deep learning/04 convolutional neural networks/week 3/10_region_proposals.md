# Region proposals

In this lesson we will cover the set of ideas called region proposals that is very influential in vision recognition.

One downside of the algorithm that we created is that it classifies a lot of regions in which there's nothing. There's a proposal to fix that made in Girshik et. al, 2013.

Rather than running the classifier in all the windows, you select a few regions and run it in there.

So you run an algorithm called segmentation algorithm in order to figure out which blobs could be objects. You may find here up to around 2000 blobs and only run algorithm in them. This has also the benefit that the regions have more flexible boxes.

However, it's still quite slow. To improve it, you would modify it a bit:

With Fast R-CNN you use convolution implementation of sliding windows to classify all the proposed regions at the same time.

The cost of propose the regions is still slow, so a different group proposed "Faster R-CCN," which uses convolutional network to propose regions too.
