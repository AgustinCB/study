1. Run the program with the following flags: `python ./process-run.py -l 5:100,5:100`. What should be the CPU utilization?

100%. The percentage of CPU usage passed to the list is 100, so there's no instruction in both processes that isn't a CPU one.

2. Now run with these flags: `python ./process-run.py -l 4:100,1:0`. How long does it take to complete the process?

We have now 4 instructions using CPU and 1 using IO. Because the IO takes 5 five units of time, the total amount of time units is 10. We have now 40% of our time in CPU and 50% in IO operations.

3. Now switch the order of flags: `python ./process-run.py -l 1:0,4:100`. What happens now? Does switching the order matter? How?

Switching operations does matter: Process one first start an IO operation. While it does that, the second process takes use of the CPU and keeps it busy still for four instruction. When it finishes, process two is also done. So the CPU utilization was 83.33% and the IO utilization was 66.67%.

4. What happens when you use `-S SWITCH_ON_END` in the previous example?

It no longer takes advantage of the fact that one process is waiting on IO and bumps the processing time to 9 time units. 

5. Now run the same processes, but with switching behavior set to switch to another process whenever one is waiting for IO. What happens now?

The same than in response 3: While the first one is doing IO, the second one can use the CPU. The total time is 6 time units.

6. What happens when you run: `python ./process-run.py -l 3:0,5:100,5:100,5:100 -S SWITCH_ON_IO -I IO_RUN_LATER -c -p`. Are resources being effectively utilized?

What happens is that during the first IO instruction, the control of the CPU passes to the next process. Once it finishes, it goes to the next one. Once it finishes, it goes to the next one. And is still then when the first program, the one with two more IO operations runs again: This time, while it's waiting on IO, there's nothing else to run and therefore the resources are poorly used.

7. Now run it with `-I IO_RUN_IMMEDIATE`. How does the behavior differ? Why might running a process that just completed a IO again be a good idea?

Now after the first IO operation ends, the control goes again to the first process. Which runs the second IO. While it does so, it passes the control to the CPU intensive processes. Once it finishes, it goes again to the first process, who runs the last IO operation. And again, the control is passed to the other process while the first one waits. This is a much more effective way of handling resources, keeping the CPU busy all the time.

It might be a good idea to hand control again to a process that just finished an IO operation because it's very possible it will have to run another one.

8. Now with some randomly generated processes: `python ./process-run.py -s 1 -l 3:50,3:50`. See if you can predict how the trace will turn out. What is the difference between running with `IO_RUN_IMMEDIATE` and `IO_RUN_LATER`? And between `SWITCH_ON_IO` and `SWITCH_ON_END`?

It's difficult to predict the CPU usage. I estimated that most outcomes would gravitate around 50% with 10% margin of error and although that was true, there were some outliers (one 34%, one 100%).

There's roughly no difference between `IO_RUN_IMMEDIATE` and `IO_RUN_LATER`.

There's a huge difference between `SWITCH_ON_IO` and `SWITCH_ON_END`, though. The later has a much worse performance, rarely passing the 40% of CPU utilization.
