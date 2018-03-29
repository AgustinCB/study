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

3. We’ll next look at the swap columns (si and so), which indicate how much swapping is taking place to and from the disk. Of course, to activate these, you’ll need to run mem with large amounts of memory. First, examine how much free memory is on your Linux system (for example, by typing cat /proc/meminfo; type man proc for details on the /proc file system and the types of information you can find there). One of the first entries in /proc/meminfo is the total amount of memory in your system. Let’s assume it’s something like 8 GB of memory; if so, start by running mem 4000 (about 4 GB) and watching the swap in/out columns. Do they ever give non-zero values? Then, try with 5000, 6000, etc. What happens to these values as the program enters the second loop (and beyond), as compared to the first loop? How much data (total) are swapped in and out during the second, third, and subsequent loops? (do the numbers make sense?)

I have 40gb of memory, which makes tests difficult.

Using 10GB of memory, causes no swap (and a segmentation fault :)). Same for using 20GB. Same for using 30GB. Testing the limits, though, seems to cause segmentation faults and not the use of swap memory.

After some fiddling, I found out that I actually didn't include any swap memory in the laptop, because well, I have 40GB. So I guess my results make sense!
