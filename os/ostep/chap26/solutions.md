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
