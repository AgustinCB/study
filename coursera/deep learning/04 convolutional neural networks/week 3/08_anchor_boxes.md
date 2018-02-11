# Anchor boxes

In this lesson we will see how to solve the problem of a grid box trying to detect two objects using anchor boxes.

Suppose we have a grid that contains two objects it's trying to detect. Because it can't pick two, it has to pick one.

What you'd is allow each grid to predict X anchor boxes. Then the output would be the values previously discussed (pc, the box and the classes) repeated X number of times in one big vector.

Previously, each object in the training set is assigned to a grid cell that contains that object's midpoint.

With X anchor boxes: Each object is assigned to a grid cell that contains object's midpoint and an anchor box for the grid cell with the highest IoU. So every objects gets assigned to a pair of grid cell and anchor box. That would also change the volume size to gxgx(sX).
