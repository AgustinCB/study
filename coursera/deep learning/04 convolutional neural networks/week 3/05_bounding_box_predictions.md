# Bounding box predictions

In this lesson we will see how to output accurate bounding boxes predictions using convolutional networks and sliding windows.


To get the best fix box possible, there's an algorithm called YOLO (you only look once), explained by Redmon et al., 2015.

Suppose that you have a 100x100. The first thing you do is divide the image in nine parts using a grid. Apply your convoluted neural network to every grid sell. For each grid cell you will have your output vector. What this does is put a middle point of each object and assign it to the grid cell that has that center.

So in this case the target output would be a 3x3x8 vector. You can do that as an extra layer using the technique we said before. If you use grids small enough, the chances of two objects in one cell is small enough.

The bounding box would be specified as follows:

- The center will be relative to the grid.

- The height and width of the grid would be specified relative to the size of the grid cell.
