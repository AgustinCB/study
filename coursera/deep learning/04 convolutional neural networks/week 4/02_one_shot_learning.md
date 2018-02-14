# One shot learning

One of the problems of face recognition is that you often need to recognize someone from only one shot. That is historically difficult and is a problem called One-shot learning. Let's see how to solve it.

One approach that you can try is feed the image of the person into a CNN and output using a softmax. This doesn't work well because with small datasets your CNN don't perform very well and it'd require to retry the network on new faces.

Instead, you can use a similarity function.

You want a function that has the following form: `d(img1, img2) = degree of difference between images`.

So if `d(img1, img2) <= \tau`, then it's the same person.
However, if `d(imag1, img2) > \tau`, then it's a different person.

You'd pass d to all the faces you want to recognise and match with the lowest difference smaller than \tau.
