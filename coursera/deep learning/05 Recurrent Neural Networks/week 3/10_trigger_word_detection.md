# Trigger word detection

In this lesson we will see how to do a trigger word system.

There's no clear literature in what is better yet, but you can just use a normal RNN. We could use the target labels 1 and 0 to specify whether or not there was a trigger word. However, this will give you a lot of unbalance (as there'd be more zeros than ones). You could set some ones around the trigger word to increase the number of ones.
