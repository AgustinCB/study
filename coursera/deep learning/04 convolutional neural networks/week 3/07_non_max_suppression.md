# Non-max suppression

In this lesson we will see how to handle multiple detections on the same object using non-max suppression.

Let's say that you want to detect three classes in an image that has two objects. Although technically they have only one midpoint and in theory they should be selected only once, in practice boxes near the center may also think that they found an object.

If many grid cells say that they detected the same object, you can clean those extra detections using non-max. The first things it does is look at the probability of the detection of every box and highlight the highest ones. Then it looks at all the other detections and all of them with a big IoU would be surpressed. That way you're left only with the highlighted ones and those are your detections.

First we run the algorithm and get our output volumn of gxgxs where g is the size of the grid matrix and s is the size of the output value.

Then we discard all boxes with pc <= 0.6. While there are any remaining boxes:

- Pick the box with the largest Pc and output that as a prediction.
- Discard any remaining box with a high overlap with the box that you just output in the previous step.
