1. Run a few randomly-generated problems with just two jobs and two queues; compute the MLFQ execution trace for each. Make your life easier by limiting the length of each job and turning off I/Os.

I ran this jobs by using:

```
$ ./mlfq.py -s 1 -n 2 -q 1 -j 2 -m 20 -M 0 -B 5
Here is the list of inputs:
OPTIONS jobs 2
OPTIONS queues 2
OPTIONS quantum length for queue  1 is   1
OPTIONS quantum length for queue  0 is   1
OPTIONS boost 5
OPTIONS ioTime 5
OPTIONS stayAfterIO False
OPTIONS iobump False
```

The first list of jobs I got is: Job 0 with runTime 3 and Job 1 with runTime 15.

In the first round, both are top priority so they run in RR. The second round, they get demoted and again run in RR. The third fifth round, job 0 finishes and job 1 gets promoted to the top. For the next 13 rounds, it will run on the top queue once, four in the bottom queue, get promoted and start again.

The response time is of 0 for Job 0 and 1 for Job 1. Average is 0.5.

The turnaround is of 5 for job 0 and 18 for job 1. The average turnaround is 11.5.

The second list of jobs is: Job 0 with runTime 19 and Job 1 with runTime 2.

In the first two rounds, both jobs run in RR in the top queue. Then they get demoted and run in RR in the second queue. At the forth time, job 1 finishes. In the fifth, Job 0 runs once in the last queue. After that and for 16 turns, Job 0 runs in the priority queue once, then four in the other queue and then gets promoted to start again.

The response time is of 0 for Job 0 and 1 for Job 1. Average is 0.5.

The turnaround is of 4 for Job 1 and 21 for Job 0. The average is 12.5.

2. How would you run the scheduler to reproduce each of the examples in the chapter?

Example 1: `./mlfq.py -n 3 -q 10 -l 0,200,0 -m 0 -M 0 -B 0 -c -S`
Example 2: `./mlfq.py -n 3 -q 10 -l 0,200,0:100,20,0 -m 0 -M 0 -B 0 -c -S`
Example 3: `./mlfq.py -n 3 -q 10 -l 20,20,1:0,200,0 -m 0 -i 9 -B 0 -c -S`
Example 4: `./mlfq.py -n 3 -q 10 -l 100,50,5:100,50,5:0,200,0 -m 0 -i 10 -B 0 -c -S`
Example 5: `./mlfq.py -n 3 -q 10 -l 100,50,5:100,50,5:0,200,0 -m 0 -i 10 -B 50 -c -S`

3. How would you configure the scheduler to behave just as RR?

I'd have only one queue, no boost time. Then the quantum slice of the scheduler would be the quantum slice of RR.

4. Craft a workload with two jobs and scheduler parameters so that one job takes advantage of the older Rules 4a and 4b (turned on with the -S flag) to game the scheduler and obtain 99% of the CPU over a particular time interval.

`./mlfq.py -n 3 -q 10 -l 0,20,9:0,200,0 -m 0 -i 1 -B 0 -c -S`

5. Given a system with a quantum length of 10 ms in its highest queue, how often would you have to boost jobs back to the highest priority level (with the -B flag) in order to guarantee that a single longrunning (and potentially-starving) job gets at least 5% of the CPU?

Every 100 ms.

6. One question that arises in scheduling is which end of a queue to add a job that just finished I/O; the -I flag changes this behavior for this scheduling simulator. Play around with some workloads and see if you can see the effect of this flag.

It's possible to game the system if you have -S activated and at the same time you put the job at the beginning of the queue after IO. That way you can get CPU even if another job is very intensive in IO by being the first one in performing a trivial IO operation.
