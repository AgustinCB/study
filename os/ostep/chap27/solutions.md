1. First build main-race.c. Examine the code so you can see the (hopefully obvious) data race in the code. Now run helgrind (by typing valgrind --tool=helgrind main-race) to see how it reports the race. Does it point to the right lines of code? What other information does it give to you?

The output:

```
==3390== Helgrind, a thread error detector
==3390== Copyright (C) 2007-2017, and GNU GPL'd, by OpenWorks LLP et al.
==3390== Using Valgrind-3.13.0 and LibVEX; rerun with -h for copyright info
==3390== Command: ./main-race
==3390== 
==3390== ---Thread-Announcement------------------------------------------
==3390== 
==3390== Thread #1 is the program's root thread
==3390== 
==3390== ---Thread-Announcement------------------------------------------
==3390== 
==3390== Thread #2 was created
==3390==    at 0x5153E6E: clone (in /usr/lib/libc-2.26.so)
==3390==    by 0x4E45DE2: create_thread (in /usr/lib/libpthread-2.26.so)
==3390==    by 0x4E47865: pthread_create@@GLIBC_2.2.5 (in /usr/lib/libpthread-2.26.so)
==3390==    by 0x4C33E97: pthread_create_WRK (hg_intercepts.c:427)
==3390==    by 0x108C64: Pthread_create (mythreads.h:51)
==3390==    by 0x108D36: main (main-race.c:14)
==3390== 
==3390== ----------------------------------------------------------------
==3390== 
==3390== Possible data race during read of size 4 at 0x30A084 by thread #1
==3390== Locks held: none
==3390==    at 0x108D37: main (main-race.c:15)
==3390== 
==3390== This conflicts with a previous write of size 4 by thread #2
==3390== Locks held: none
==3390==    at 0x108CEF: worker (main-race.c:8)
==3390==    by 0x4C34096: mythread_wrapper (hg_intercepts.c:389)
==3390==    by 0x4E4708B: start_thread (in /usr/lib/libpthread-2.26.so)
==3390==    by 0x5153E7E: clone (in /usr/lib/libc-2.26.so)
==3390==  Address 0x30a084 is 0 bytes inside data symbol "balance"
==3390== 
==3390== ----------------------------------------------------------------
==3390== 
==3390== Possible data race during write of size 4 at 0x30A084 by thread #1
==3390== Locks held: none
==3390==    at 0x108D40: main (main-race.c:15)
==3390== 
==3390== This conflicts with a previous write of size 4 by thread #2
==3390== Locks held: none
==3390==    at 0x108CEF: worker (main-race.c:8)
==3390==    by 0x4C34096: mythread_wrapper (hg_intercepts.c:389)
==3390==    by 0x4E4708B: start_thread (in /usr/lib/libpthread-2.26.so)
==3390==    by 0x5153E7E: clone (in /usr/lib/libc-2.26.so)
==3390==  Address 0x30a084 is 0 bytes inside data symbol "balance"
==3390== 
==3390== 
==3390== For counts of detected and suppressed errors, rerun with: -v
==3390== Use --history-level=approx or =none to gain increased speed, at
==3390== the cost of reduced accuracy of conflicting-access information
==3390== ERROR SUMMARY: 2 errors from 2 contexts (suppressed: 0 from 0)
```

Yes, it's correct and points to the correct lines.

It also tells me that it's a data race problem, it spots the two threads that are part of it, it informs that there's no lock being held and it probides with the address that's being accessed more than once. Pretty cool.

2. What happens when you remove one of the offending lines of code? Now add a lock around one of the updates to the shared variable, and then around both. What does helgrind report in each of these cases?

Commenting line 8 reports no error.

Same for using locks.
