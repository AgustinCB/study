1.- 

It is inefficient because if the size of addAll is big enough, we will end up calling resize multiple times. However, we have enough information to perform only one allocation.

Same problem with moving the array to the right: We can do that in one step, by moving it n steps to the right instead of one.
