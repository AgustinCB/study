# Object localization

In this lesson we will see what object localization is.

We have already see what image classification is: To tell whether the item of a picture is something or not.

Classification with localization is about also putting a picture around the category.

Detection would be to detect all the desired items in a picture.

The first two problems usually have one big object in the middle that we try to localize and regonise. In the third, we have multiple objects.

## Classification with localization

For image classification we may input a picture into a ConvNet with multiple layers which results into a set of pictures that is sent into a softmax unit which classifies the picture,

To also localize the car, we can have two output units: One using softmax and another one that spits the bounding box of the object in the form of the center (bx, by) and the height and width (bh, bw). All this  will be a supervised learning task.

So let's define the target label as:

```
y = [Pc bx by bh bw]
```

Where:

```
Pc = is there any object?
bx = x axis of the center
by = y axis of the center
bw = width of the box
bh = height of the box
C1 = class one
...
cn = class n
```

If there's no object in the image, Pc=0 and the rest of values would be undefined (we don't really care about them).

Our loss function would be something like:

```
l(y', y) = sum(1, size(y), i, (y'[i] - y[i])^2) if y[1] = 1
         = (y'[1] - y[1])^2 if y[1] = 0
```
