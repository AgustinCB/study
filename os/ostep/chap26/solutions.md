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
