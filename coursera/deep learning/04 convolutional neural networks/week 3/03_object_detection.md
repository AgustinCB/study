# Object detection

In this lesson we will see how an object detection algorithm would work using a ConvNet and sliding window.

Let's say that we want to build a card detection algorithm. We could start with closely cropped images and see if they are cars or not. Once we have that, we could use it in sliding window detection.

So having a big image, we will select an small region in one corner of the image and feed it to the convnet to see if it's or not a car. We would then move the window a little bit and check it again, repeating still you went through all the image and know all the subimages that are a car.

Having done this, you repeat the process but using a bigger image. Repeating this process again some times hoping that you collect all the possible cars in the image.

This has a very huge computational cost.
