# Broadcasting in Python

In this lesson, let's see how broadcasting in Python works.

Suppose we have the number of calories from carbs, proteins and fats in 100g of different foods.

For example:
	Apples	Beef	Eggs	Potatoes
Carbs	56.0	0.0	4.4	68.0
Protein	1.2	104.0	52.0	8.0
Fat	1.8	135.0	99.0	0.9

Can you get the percerntage of calories for ccarbs, protein and fat without using an explicit for loop?

To do so, we will define a (3, 4) matrix A.

```python
A = np.array([56, 0, 4.4, 68.0], [1.2, 104.0, 52.0, 8.0], [1.8, 135.0,99.0,0.9])

cal = A.sum(axis=0)
# [59, 239, 155.4, 76.9]
percentage = 100 * A/(cal.reshape(1/4))
```

The last command is an example of Python broadcasting: We are dividing a matrix of (3/4) by vector (1,4). How do we do this?

If you do: `[1, 2, 3, 4] + 100` in python, the engine will expand the 100 number into a matrix like [100, 100, 100, 100] and then sum.

Similarly, if we had: `[[1, 2, 3], [4, 5, 6]] + [100, 200, 300]`, python will expand the second matrix to add a second row with the same values at the second one. This is exactly what we did in our percentage calculation. In every case, the operation that we do is element wide (and not a matrix operation).
