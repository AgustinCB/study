# Whether to use end-to-end learning

In this lesson we will see how to decide on whether or not to use end-to-end learning.

Some of the benefits of applying end-to-end learning:

- It lets the data speak. It doesn't need to reflect human preconception. It can just focus on the statistical characteristics of the data.
- Less hand-designing of components needed. You don't need to spend a lot of time manually dealing with some problems.

And now some of its problems:

- May need large amounts of data.
- Excludes potentially useful hand-designed components. Hand-designed components can be a way to inject previous knowledge in your algorithm.

You can think of a machine learning system as a mix of data and hand-design components. If you have enough data, you don't need as much hand-design components. On the other hand, on bigger datasets, hand-design components would hurt more than help.

So the key question is: Do you have sufficient data to learn a function of the complexity needed to map your input with your output?
