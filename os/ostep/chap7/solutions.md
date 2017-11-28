1. Compute the response time and turnaround when running three jobs of length 200 with the SJF and FIFO schedulers.

For FIFO:

a) Turnaround: Firt job has a turnaround of 200. The second of 400. The third one of 600. The average turnaround is 400.
b) Response time: First job has a response time of 0. The second one of 200. The third one of 400. The average response time is 200.

For SJF:

a) Turnaround: Firt job has a turnaround of 200. The second of 400. The third one of 600. The average turnaround is 400.
b) Response time: First job has a response time of 0. The second one of 200. The third one of 400. The average response time is 200.

2. Now do the the same, but with jobs of length 100, 200 and 300.

For FIFO:

a) Turnaround: First job has a turnaround of 100. The second one of 300. The third one of 600. The average turnaround is 333.33.
b) Response time: First job has a response time of 0. The second one of 100. The third one of 300. The average response time is 133.33.

For SJF:

a) Turnaround: First job has a turnaround of 100. The second one of 300. The third one of 600. The average turnaround is 333.33.
b) Response time: First job has a response time of 0. The second one of 100. The third one of 300. The average response time is 133.33.

3. Now do the same, but with the RR scheduler and a time slice of 1.

a) Turnaround: First job has a turnaround of 298. The second one of 499. The third one of 600. The average turnaround is 465.66.
b) Response time: First job has a response time of 0. The second one of 1. The third one of 2. The average response time is 1.

4. For what type of workloads does SJF deliver the same turnaround times as FIFO?

For workloads that are either already sorted or in which the jobs have the same length.

5. For what type of workloads and quantum lengths does SJF deliver the same response time as RR?

For workloads where all the jobs have the same length and the time slice is that length.

6. What happens to response time with SJF as job lengths increase? Can you use the simulator to demonstrate the trend?

It increases.

7. What happens to response time with RR as quantum lengths increase? Can you write an equation that gives the worst-case average response time, given N jobs?

It increases.

f(jobs, n) = (jobs[0] * ((n-1) * n)/2) / n
