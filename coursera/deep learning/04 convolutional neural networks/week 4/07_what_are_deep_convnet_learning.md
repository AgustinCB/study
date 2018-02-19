# What are deep ConvNets learning?

In this less we will see what are convolutional networks learning in every layer to apply them to neural style transfer. See also Zeiler and Fergus, 2013.

First pick a unit in layer one. Find the nine image patches that maximize the unit's activation for a given input. Then repeat the process for all the other units. This gives you a sense of what units are "looking for."

In deeper layers the hidden units will see larger image patches. You'd repeat the process for them. They will tend to have more information as you go deeper.
