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

2. Let’s now start looking at some of the memory statistics while running mem. We’ll focus on two columns: swpd (the amount of virtual memory used) and free (the amount of idle memory). Run ./mem 1024 (which allocates 1024 MB) and watch how these values change. Then kill the running program (by typing control-c) and watch again how the values change. What do you notice about the values? In particular, how does the free column change when the program exits? Does the amount of free memory increase by the expected amount when mem exits?

It started with 8.6G of free memory. As I started the mem program, it dropped to 7.5G, which kinda makes sense, it's approximately the size we are allocating.

Similarly, when I stopped the mem program, the free memory went up to 8.6G again. Repeating the experiment gave the same results, which confirms that the world is a beautiful place where things make sense.
