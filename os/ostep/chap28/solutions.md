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
