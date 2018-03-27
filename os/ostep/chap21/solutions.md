# Beyond Physical memory

1. We will run the program mem.c but with very little memory
usage. This can be accomplished by typing ./mem 1 (which uses
only 1 MB of memory). How do the CPU usage statistics change
when running mem? Do the numbers in the user time column
make sense? How does this change when running more than one
instance of mem at once?

The free memory goes down slightly. It went from 6956892 to 6932092 and stayed there for some seconds, decreasing very slowly over time.

Same happened when adding the second process: It went from 6928572 to 6921156 and kept slowly decreasing.

The numbers in user time make sense, they change a lot in the second you added a process and then kinda go down to the previous date range, which makes, since there's a lot of initialization to be done at the beginning and after that a bunch of IO.

A proof of that is that also the system time slightly goes up when the user time decreased again. Same happened when adding the second process.
