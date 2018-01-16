# Carrying out error analysis

In this lesson we will see how manually examining errors your algorithm makes can help.

Let's say you have cat classifier with 10% error. You notice that it classifies certain dogs as cats. Should you try to make your cat classifier do better on dogs?

To answer that, you should:

- Get about 100 mislabeled dev set examples.
- Count up how many are dogs.

If about 5% of those are dogs, then your error would go down from 10% to 9.5% which isn't that much.

Suppose that you find that 50% of them are dogs. In this case, your error would go down from 10% to 5%, a much promising result.

Sometimes you can evaluate multiple ideas in parallel.

For example, suppose you have this ideas:

- Fix pictures of dogs being recognized as cats.
- Fix great cats being misrecognized.
- Improve performance on blurry images.

You could then create a table:

Image	Dog	Great cat	Blurry	Comments
1	1	0		0	Pitbull
2	0	0		1
3	0	1		1	Rainy day at zoo
...
% total	8%	43%		61%

This would make easy to see what's the biggest problem and how worth would be to work on every of this examples.
