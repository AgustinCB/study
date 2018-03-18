# Vanishing gradients with RNNs

In this lesson we will see how to address the vanishing gradients problem with RNNs.

Language can have very long dependencies, in the sense that a word that appears very early may affect later words. However, the architecture we have seen so far is very bad at this kind of cases: In long sentences, vanishing gradients will forget that information.

This is a weakness that needs to be addressed in order to be able to work properly with big sentences. This is also harder to see than exploding gradients, which is usually very evident and it appears as a lot of NaNs in the parameters. A solution is to use gradient clipping.
