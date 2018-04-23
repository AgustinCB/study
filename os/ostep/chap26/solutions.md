1. To start, let’s examine a simple program, “loop.s”. First, just look at the program, and see if you can understand it: cat loop.s. Then, run it with these arguments: ./x86.py -p loop.s -t 1 -i 100 -R dx. This specifies a single thread, an interrupt every 100 instructions,  and tracing of register %dx. Can you figure out what the value of %dx will be during the run? Once you have, run the same above and use the -c flag to check your answers; note the answers, on the left, show the value of the register (or memory value) after the instruction on the right has run.

I was able to imagine them relative to the initial value. I wasn't sure at which value the register would start. I suspected zero, but I wouldn't be surprised if it was a random number.

```
   dx          Thread 0         
    0   
   -1   1000 sub  $1,%dx
   -1   1001 test $0,%dx
   -1   1002 jgte .top
   -1   1003 hal
```

2. Now run the same code but with these flags: `./x86.py -p loop.s -t 2 -i 100 -a dx=3,dx=3 -R dx` This specifies two threads, and initializes each %dx register to 3. What values will %dx see? Run with the -c flag to see the answers. Does the presence of multiple threads affect anything about your calculations? Is there a race condition in this code?

There's anything special to keep in mind when calculating, nor race condition. The only trick is to know that every thread's register values are separated and they shouldn't interefer with each other.

```
ARG seed 0
ARG numthreads 2
ARG program loop.s
ARG interrupt frequency 100
ARG interrupt randomness False
ARG argv dx=3,dx=3
ARG load address 1000
ARG memsize 128
ARG memtrace 
ARG regtrace dx
ARG cctrace False
ARG printstats False
ARG verbose False

   dx          Thread 0                Thread 1         
    3   
    2   1000 sub  $1,%dx
    2   1001 test $0,%dx
    2   1002 jgte .top
    1   1000 sub  $1,%dx
    1   1001 test $0,%dx
    1   1002 jgte .top
    0   1000 sub  $1,%dx
    0   1001 test $0,%dx
    0   1002 jgte .top
   -1   1000 sub  $1,%dx
   -1   1001 test $0,%dx
   -1   1002 jgte .top
   -1   1003 halt
    3   ----- Halt;Switch -----  ----- Halt;Switch -----  
    2                            1000 sub  $1,%dx
    2                            1001 test $0,%dx
    2                            1002 jgte .top
    1                            1000 sub  $1,%dx
    1                            1001 test $0,%dx
    1                            1002 jgte .top
    0                            1000 sub  $1,%dx
    0                            1001 test $0,%dx
    0                            1002 jgte .top
   -1                            1000 sub  $1,%dx
   -1                            1001 test $0,%dx
   -1                            1002 jgte .top
   -1                            1003 halt
```

3. Now run the following: `./x86.py -p loop.s -t 2 -i 3 -r -a dx=3,dx=3 -R dx` This makes the interrupt interval quite small and random; use different seeds with -s to see different interleavings. Does the frequency of interruption change anything about this program?

Kinda.

```
   dx          Thread 0                Thread 1         
    3   
    2   1000 sub  $1,%dx
    2   1001 test $0,%dx
    2   1002 jgte .top
    3   ------ Interrupt ------  ------ Interrupt ------  
    2                            1000 sub  $1,%dx
    2                            1001 test $0,%dx
    2                            1002 jgte .top
    2   ------ Interrupt ------  ------ Interrupt ------  
    1   1000 sub  $1,%dx
    1   1001 test $0,%dx
    2   ------ Interrupt ------  ------ Interrupt ------  
    1                            1000 sub  $1,%dx
    1   ------ Interrupt ------  ------ Interrupt ------  
    1   1002 jgte .top
    0   1000 sub  $1,%dx
    1   ------ Interrupt ------  ------ Interrupt ------  
    1                            1001 test $0,%dx
    1                            1002 jgte .top
    0   ------ Interrupt ------  ------ Interrupt ------  
    0   1001 test $0,%dx
    0   1002 jgte .top
   -1   1000 sub  $1,%dx
    1   ------ Interrupt ------  ------ Interrupt ------  
    0                            1000 sub  $1,%dx
   -1   ------ Interrupt ------  ------ Interrupt ------  
   -1   1001 test $0,%dx
   -1   1002 jgte .top
    0   ------ Interrupt ------  ------ Interrupt ------  
    0                            1001 test $0,%dx
    0                            1002 jgte .top
   -1   ------ Interrupt ------  ------ Interrupt ------  
   -1   1003 halt
    0   ----- Halt;Switch -----  ----- Halt;Switch -----  
   -1                            1000 sub  $1,%dx
   -1                            1001 test $0,%dx
   -1   ------ Interrupt ------  ------ Interrupt ------  
   -1                            1002 jgte .top
   -1                            1003 halt
```

It doesn't change the behaviour of each thread, but it jumps more often from one to another.

4. Next we’ll examine a different program (looping-race-nolock.s). This program accesses a shared variable located at memory address 2000; we’ll call this variable value for simplicity. Run it with a single thread and make sure you understand what it does, like this: `./x86.py -p looping-race-nolock.s -t 1 -M 2000`. What value is found in value (i.e., at memory address 2000) throughout the run? Use -c to check your answer.


```
 2000          Thread 0         
    0
    0   1000 mov 2000, %ax
    0   1001 add $1, %ax
    1   1002 mov %ax, 2000
    1   1003 sub  $1, %bx
    1   1004 test $0, %bx
    1   1005 jgt .top
    1   1006 halt
```

5. Now run with multiple iterations and threads: `./x86.py -p looping-race-nolock.s -t 2 -a bx=3 -M 2000` Do you understand why the code in each thread loops three times? What will the final value of value be?

Yeah, because the register bx contains the number of iterations and registers aren't shared across threads, but they have their own values (both initialized at zero). The final value would be 6.

6. Now run with random interrupt intervals: `./x86.py -p looping-race-nolock.s -t 2 -M 2000 -i 4 -r -s 0`. Then change the random seed, setting -s 1, then -s 2, etc. Can you tell, just by looking at the thread interleaving, what the final value of value will be? Does the exact location of the interrupt matter? Where can it safely occur? Where does an interrupt cause trouble? In other words, where is the critical section exactly?

Yes, I can tell just by looking at the thread intervaling what the final value will be, although it's hard to follow because the place matter a lot. If the interruption happens before the value is put back in memory, two threads may increase the same value at the same time, reducing the total value expected at the end. The problem would be interrupting in 1001.

7. Now use a fixed interrupt interval to explore the program further. Run: `./x86.py -p looping-race-nolock.s -a bx=1 -t 2 -M 2000 -i 1`. See if you can guess what the final value of the shared variable value will be. What about when you change -i 2, -i 3, etc.? For which interrupt intervals does the program give the “correct” final answer?

With `-i 1` the final value is 1. With 2 will be one too. With 3, it will then be the correct answer (2).

8. Now run the same code for more loops (e.g., set -a bx=100). What interrupt intervals, set with the -i flag, lead to a “correct” outcome? Which intervals lead to surprising results?

It works similar to exercise 7. Intervals that are multiple of three lead to a correct outcome. The rest, lead to incorrect outcome.
