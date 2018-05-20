1. First let’s get ready to run x86.py with the flag -p flag.s. This code “implements” locking with a single memory flag. Can you understand what the assembly code is trying to do?

Yeah, it's a loop that runs %bx times. In each iteration, it checks if a lock is acquired. If it's, it checks again. If not, it adds one to a variable (count).

To check if a lock is acquired, it uses another variable: Flag. If it's zero, lock is free, if it's different than zero, is acquired

2. When you run with the defaults, does flag.s work as expected? Does it produce the correct result? Use the -M and -R flags to trace variables and registers (and turn on -c to see their values). Can you predict what value will end up in flag as the code runs?

Yeah, it does behave as expected.

```
 flag count      bx    ax          Thread 0                Thread 1         

    0     0       0     0   
    0     0       0     0   1000 mov  flag, %ax
    0     0       0     0   1001 test $0, %ax
    0     0       0     0   1002 jne  .acquire
    1     0       0     0   1003 mov  $1, flag
    1     0       0     0   1004 mov  count, %ax
    1     0       0     1   1005 add  $1, %ax
    1     1       0     1   1006 mov  %ax, count
    0     1       0     1   1007 mov  $0, flag
    0     1      -1     1   1008 sub  $1, %bx
    0     1      -1     1   1009 test $0, %bx
    0     1      -1     1   1010 jgt .top
    0     1      -1     1   1011 halt
    0     1       0     0   ----- Halt;Switch -----  ----- Halt;Switch -----  
    0     1       0     0                            1000 mov  flag, %ax
    0     1       0     0                            1001 test $0, %ax
    0     1       0     0                            1002 jne  .acquire
    1     1       0     0                            1003 mov  $1, flag
    1     1       0     1                            1004 mov  count, %ax
    1     1       0     2                            1005 add  $1, %ax
    1     2       0     2                            1006 mov  %ax, count
    0     2       0     2                            1007 mov  $0, flag
    0     2      -1     2                            1008 sub  $1, %bx
    0     2      -1     2                            1009 test $0, %bx
    0     2      -1     2                            1010 jgt .top
    0     2      -1     2                            1011 halt
```

3. Change the value of the register %bx with the -a flag (e.g., -a bx=2,bx=2 if you are running just two threads). What does the code do? How does it change your answer for the question above?

Doesn't change my answer. With two, all it does is adding more iterations. My answer would probably change at the point in which the number of iterations is big enough to be able to interrupt one thread before it halts.

4. Set bx to a high value for each thread, and then use the -i flag to generate different interrupt frequencies; what values lead to a bad outcomes? Which lead to good outcomes?

First, `-i` has to be smaller than `bx` * n, where n is the number of instructions in the program. The smaller `-i`, you're more likely to screw things up by interruptingin the wrong place. The problem is in the step `mov flag, %ax` in particular. If you interrupt in such a way that two threads access that at the same time, you have a problem. Such a problem can happen if the program jumps just after `jne  .acquire` and before setting one to flag. Then that hacky lock is overruled.

You can achieve that by setting bx for both threads to five and the `-i` to five too. Then the total you get in count is 9 instead of 10 because of this race condition.

5. Now let’s look at the program test-and-set.s. First, try to understand the code, which uses the xchg instruction to build a simple locking primitive. How is the lock acquire written? How about lock release?

The problem with the previous code is that we required two mov related with flag to check the mutex and an interruption in hte middle could mess things up.

By using xchg, that problem is solved: We set ax to 1 and then exchange with `mutex`. If after that ax is set 0, it means that the lock was free and we acquired it (because the previous value of mutex was zero), so we can keep going.

However, if the lock wasn't free, xchg would return back 1 to ax, in which case we'd just try again. Easy and for all the family.

The release of the lock is simpler because it just needs one operation (mov 0, mutex) that is guaranteed to be atomic. So nothing fancy needed.

6. Now run the code, changing the value of the interrupt interval (-i) again, and making sure to loop for a number of times. Does the code always work as expected? Does it sometimes lead to an inefficient use of the CPU? How could you quantify that?

Yeah, it's consistently correct, as I expected in the previous answer. But it's true that's also inefficient, depending on the value of -i. It seems like the minimum is 112 instructions, but if you switch in the correct moment you can end up doing about 150 instructions, of which about 40 is just seeing we can get the lock.

I'd measure it as the proportion of instructions spent spinning in the acquire loop instead of doing something useful.

7. Use the -P flag to generate specific tests of the locking code. For example, run a schedule that grabs the lock in the first thread, but then tries to acquire it in the second. Does the right thing happen? What else should you test?

Yes, it keeps working at expected. Another good test to run would be to check that there's no way to mess up the release of the lock.

8. Now let’s look at the code in peterson.s, which implements Peterson’s algorithm (mentioned in a sidebar in the text). Study the code and see if you can make sense of it.

First it defines some variables.

Then it loads the address of the flag array into a register (fx). Assuming bx is the id of the thread (and that there's two threads and their ids can be 0 or 1), put the id of the other thread in cx.

Set flag[self] = 1 and move the id of the other thread to the variable turn.

Then move the flag of the other thread to ax and enter the critical section if it isn't hold.
If it isn't, move turn to ax and check if it's still the other threads turn, if so, go ahead in keep try again if the other thread's hold isn't hold. If not, move into the critical section.

Finally, set your flag to zero and move the turn to the other thread.

It will work well that combination of two flags plus a turn variable.

9. Now run the code with different values of -i. What kinds of different behavior do you see? Make sure to set the thread IDs appropriately (using -a bx=0,bx=1 for example) as the code assumes it.

Past 15 as -i, the result is consistent: Executes each thread in turns, finishes in 30 instructions and with the expected result.

With numbers smaller than 15, the number of instructions is bigger. With 5, for example, is about 54, spending a lot of time waiting to get a turn (more than 44% of cpu time).

Other than that, the result is consistently what you'd expect.

10. Can you control the scheduling (with the -P flag) to “prove” that the code works? What are the different cases you should show hold? Think about mutual exclusion and deadlock avoidance.

Yes, I can. I could arrange the values so that the interruption happens in the places right after memory is being moved to registers and check that the sky didn't break. For example, when moving the turn or count variables. It works.

11. Now study the code for the ticket lock in ticket.s. Does it match the code in the chapter?

Yes, it works as stated in the chapter.

12. Now run the code, with the following flags: -a bx=1000,bx=1000 (this flag sets each thread to loop through the critical 1000 times). Watch what happens over time; do the threads spend much time spinning waiting for the lock?

YES. With the default interruption, it does 99463 instructions. Then I decided to do an optimal interruption (i.e. the number of instructions in the program before the halt): 26002 instructions. That means that it spent approximately 75% of it's cpu time spinning. Terrible, terrible.

13. How does the code behave as you add more threads?

With five threads, the default interruption value yields 249148 instructions, while the optimal 65005, approximately the same ratio. I decided to make a little graph and it shows that the ratio of suboptimal time in cpu stays approximately the same after passing the ideal number of instructions before an interruption. Always about 75%. Which is terrible, by the way.
