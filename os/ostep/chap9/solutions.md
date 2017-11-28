1.

With seed 1:

```
$ ./lottery.py -j 3 -s 1
ARG jlist 
ARG jobs 3
ARG maxlen 10
ARG maxticket 100
ARG quantum 1
ARG seed 1

Here is the job list, with the run time of each job: 
  Job 0 ( length = 1, tickets = 84 )
  Job 1 ( length = 7, tickets = 25 )
  Job 2 ( length = 4, tickets = 44 )


Here is the set of random numbers you will need (at most):
Random 651593 -> Winner ticket is 119, Job 2 runs and its length is now 3.
Random 788724 -> Winner ticket is 9, job 0 runs and its length is now 0. Job finished.
Random 93859 -> Winner ticket is 19, job 1 runs and its length is 5 now.
Random 28347 -> Winner ticket is 57, Job 2 runs and its length is 2 now.
Random 835765 -> Winner ticket is 37,  Job 2 runs and its length is 1 now.
Random 432767 -> Winner ticket is 68, Job 2 runs and its length is 0 now.
Random 762280 -> Job 1 runs, length is 6 now.
Random 2106 -> Job 1 runs, length is 5 now.
Random 445387 -> Job 1 runs, length is 4 now.
Random 721540 -> Job 1 runs, length is 3 now.
Random 228762 -> Job 1 runs, length is 2 now.
Random 94527 -> Job 1 runs, length is 1 now.
```

With seed 2:

```
$ ./lottery.py -j 3 -s 2
ARG jlist 
ARG jobs 3
ARG maxlen 10
ARG maxticket 100
ARG quantum 1
ARG seed 2

Here is the job list, with the run time of each job: 
  Job 0 ( length = 9, tickets = 94 )
  Job 1 ( length = 8, tickets = 73 )
  Job 2 ( length = 6, tickets = 30 )


Here is the set of random numbers you will need (at most):
Random 605944 -> Winner ticket is 169, Job 2 runs and its length is 5 now.
Random 606802 -> Winner ticket is 42, Job 0 runs and its length is 8 now.
Random 581204 -> Winner ticket is 54, Job 0 runs and its length is 7 now.
Random 158383 -> Winner ticket is 192, Job 2 runs and its length is 4 now.
Random 430670 -> Winner ticket is 28, Job 0 runs and its length is 6 now.
Random 393532 -> Winner ticket is 123, Job 1 runs and its length is 7 now.
Random 723012 -> Winner ticket is 22, Job 0 runs and its length is 5 now.
Random 994820 -> Winner ticket is 167, Job 2 runs and its length is 3 now.
Random 949396 -> Winner ticket is 53, Job 0 runs and its length is 4 now.
Random 544177 -> Winner ticket is 63, Job 0 runs and its length is 3 now.
Random 444854 -> Winner ticket is 28, Job 0 runs and its length is 2 now.
Random 268241 -> Winner ticket is 124, Job 1 runs and its length is 6 now.
Random 35924 -> Winner ticket is 70, Job 0 runs and its length is 1 now.
Random 27444 -> Winner ticket is 61, Job 0 runs and its length is 0 now. Job 0 finished.
Random 464894 -> Winner ticket is 55, Job 1 runs and its length is 5 now.
Random 318465 -> Winner ticket is 92, Job 2 runs and its length is 2 now.
Random 380015 -> Winner ticket is 48, Job 1 runs and its length is 4 now.
Random 891790 -> Winner ticket is 16, Job 1 runs and its length is 3 now.
Random 525753 -> Winner ticket is 41, Job 1 runs and its length is 2 now.
Random 560510 -> Winner ticket is 87, Job 2 runs and its length is 1 now.
Random 236123 -> Winner ticket is 47, Job 1 runs and its length is 1 now.
Random 23858 -> Winner ticket is 65, Job 1 runs and its length is 0 now. Job 1 finished/
Random 325143 -> Job 2 runs and finishes.
```

With seed 3:

```
$ ./lottery.py -j 3 -s 3
ARG jlist 
ARG jobs 3
ARG maxlen 10
ARG maxticket 100
ARG quantum 1
ARG seed 3

Here is the job list, with the run time of each job: 
  Job 0 ( length = 2, tickets = 54 )
  Job 1 ( length = 3, tickets = 60 )
  Job 2 ( length = 6, tickets = 6 )


Here is the set of random numbers you will need (at most):
Random 13168 -> Winner ticket is 88, Job 1 runs and its length is 2 now.
Random 837469 -> Winner ticket is 109, Job 1 runs and its length is 1 now.
Random 259354 -> Winner ticket is 34, Job 0 runs and its length is 1 now.
Random 234331 -> Winner ticket is 91, Job 1 runs and its length is 0 now. Job 1 finishes.
Random 995645 -> Winner ticket is 5, Job 0 runs and its length is 0 now. Job 0 finishes.
Random 470263 -> Job 2 runs.
Random 836462 -> Job 2 runs.
Random 476353 -> Job 2 runs.
Random 639068 -> Job 2 runs.
Random 150616 -> Job 2 runs.
Random 634861 -> Job 2 runs.
```

2. Now run with two specific jobs: each of length 10, but one (job 0) with just 1 ticket and the other (job 1) with 100 (e.g., -l 10:1,10:100). What happens when the number of tickets is so imbalanced? Will job 0 ever run before job 1 completes? How often? In general, what does such a ticket imbalance do to the behavior of lottery scheduling?

What would happen is that the job with 100 will use most of the cpu and relegate the other one to the end, while using only 1% of the CPU meanwhile. This will hurt both the response time and the turnover of the algorithm.

3. When running with two jobs of length 100 and equal ticket allocations of 100 (-l 100:100,100:100), how unfair is the scheduler? Run with some different random seeds to determine the (probabilistic) answer; let unfairness be determined by how much earlier one job finishes than the other.

In such conditions, the scheduler isn't unfair at all, giving half the cpu time to each job (roughly).

4. How does your answer to the previous question change as the quantum size (-q) gets larger?

It becomes less fair as the quantum size increases. The job that runs first gets an advantage.

5. Can you make a version of the graph that is found in the chapter? What else would be worth exploring? How would the graph look with a stride scheduler?

The graph would look fairer. Although with quantum size major or equals to the first job, there wouldn't be any difference.
