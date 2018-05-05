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

3. Now let’s look at main-deadlock.c. Examine the code. This code has a problem known as deadlock (which we discuss in much more depth in a forthcoming chapter). Can you see what problem it might have?

Yes. Suppose thread 1 arrives to line 10, gets the lock and then there's a context switch to thread 2. Now thread 2 arrives to line 13, gets the other lock and goes to wait for the first lock. Thread 1, on the other hand, is waiting for the lock that thread 2 just got. There'll be waiting forever.

4. Now run helgrind on this code. What does helgrind report?

It says this:

```
==1684== ---Thread-Announcement------------------------------------------
==1684== 
==1684== Thread #3 was created
==1684==    at 0x5153E6E: clone (in /usr/lib/libc-2.26.so)
==1684==    by 0x4E45DE2: create_thread (in /usr/lib/libpthread-2.26.so)
==1684==    by 0x4E47865: pthread_create@@GLIBC_2.2.5 (in /usr/lib/libpthread-2.26.so)
==1684==    by 0x4C33E97: pthread_create_WRK (hg_intercepts.c:427)
==1684==    by 0x108C64: Pthread_create (mythreads.h:51)
==1684==    by 0x108D99: main (main-deadlock.c:24)
==1684== 
==1684== ----------------------------------------------------------------
==1684== 
==1684== Thread #3: lock order "0x30A0A0 before 0x30A0E0" violated
==1684== 
==1684== Observed (incorrect) order is: acquisition of lock at 0x30A0E0
==1684==    at 0x4C314AC: mutex_lock_WRK (hg_intercepts.c:912)
==1684==    by 0x108AE7: Pthread_mutex_lock (mythreads.h:23)
==1684==    by 0x108D16: worker (main-deadlock.c:13)
==1684==    by 0x4C34096: mythread_wrapper (hg_intercepts.c:389)
==1684==    by 0x4E4708B: start_thread (in /usr/lib/libpthread-2.26.so)
==1684==    by 0x5153E7E: clone (in /usr/lib/libc-2.26.so)
==1684== 
==1684==  followed by a later acquisition of lock at 0x30A0A0
==1684==    at 0x4C314AC: mutex_lock_WRK (hg_intercepts.c:912)
==1684==    by 0x108AE7: Pthread_mutex_lock (mythreads.h:23)
==1684==    by 0x108D22: worker (main-deadlock.c:14)
==1684==    by 0x4C34096: mythread_wrapper (hg_intercepts.c:389)
==1684==    by 0x4E4708B: start_thread (in /usr/lib/libpthread-2.26.so)
==1684==    by 0x5153E7E: clone (in /usr/lib/libc-2.26.so)
==1684== 
==1684== Required order was established by acquisition of lock at 0x30A0A0
==1684==    at 0x4C314AC: mutex_lock_WRK (hg_intercepts.c:912)
==1684==    by 0x108AE7: Pthread_mutex_lock (mythreads.h:23)
==1684==    by 0x108CFC: worker (main-deadlock.c:10)
==1684==    by 0x4C34096: mythread_wrapper (hg_intercepts.c:389)
==1684==    by 0x4E4708B: start_thread (in /usr/lib/libpthread-2.26.so)
==1684==    by 0x5153E7E: clone (in /usr/lib/libc-2.26.so)
==1684== 
==1684==  followed by a later acquisition of lock at 0x30A0E0
==1684==    at 0x4C314AC: mutex_lock_WRK (hg_intercepts.c:912)
==1684==    by 0x108AE7: Pthread_mutex_lock (mythreads.h:23)
==1684==    by 0x108D08: worker (main-deadlock.c:11)
==1684==    by 0x4C34096: mythread_wrapper (hg_intercepts.c:389)
==1684==    by 0x4E4708B: start_thread (in /usr/lib/libpthread-2.26.so)
==1684==    by 0x5153E7E: clone (in /usr/lib/libc-2.26.so)
==1684== 
==1684==  Lock at 0x30A0A0 was first observed
==1684==    at 0x4C314AC: mutex_lock_WRK (hg_intercepts.c:912)
==1684==    by 0x108AE7: Pthread_mutex_lock (mythreads.h:23)
==1684==    by 0x108CFC: worker (main-deadlock.c:10)
==1684==    by 0x4C34096: mythread_wrapper (hg_intercepts.c:389)
==1684==    by 0x4E4708B: start_thread (in /usr/lib/libpthread-2.26.so)
==1684==    by 0x5153E7E: clone (in /usr/lib/libc-2.26.so)
==1684==  Address 0x30a0a0 is 0 bytes inside data symbol "m1"
==1684== 
==1684==  Lock at 0x30A0E0 was first observed
==1684==    at 0x4C314AC: mutex_lock_WRK (hg_intercepts.c:912)
==1684==    by 0x108AE7: Pthread_mutex_lock (mythreads.h:23)
==1684==    by 0x108D08: worker (main-deadlock.c:11)
==1684==    by 0x4C34096: mythread_wrapper (hg_intercepts.c:389)
==1684==    by 0x4E4708B: start_thread (in /usr/lib/libpthread-2.26.so)
==1684==    by 0x5153E7E: clone (in /usr/lib/libc-2.26.so)
==1684==  Address 0x30a0e0 is 0 bytes inside data symbol "m2"
==1684== 
==1684== 
==1684== 
==1684== For counts of detected and suppressed errors, rerun with: -v
==1684== Use --history-level=approx or =none to gain increased speed, at
==1684== the cost of reduced accuracy of conflicting-access information
==1684== ERROR SUMMARY: 1 errors from 1 contexts (suppressed: 7 from 7)
```

It basically is complaining for not following an expected lock order. Fair enough, it's not.

5. Now run helgrind on main-deadlock-global.c. Examine the code; does it have the same problem that main-deadlock.c has? Should helgrind be reporting the same error? What does this tell you about tools like helgrind?

No, it doesn't have the same problem. The use of an extra lock to ensure the whole section is thread safe removes the chance of a deadlock. However, helgrind reports the same error. This makes evident the limitations of the tool, which can't probably report with complete accuracy all cases and decides to take a conservative approach (i.e. when it can't distinguish if a case is wrong or not, assume it's wrong).

6. Let’s next look at main-signal.c. This code uses a variable (done) to signal that the child is done and that the parent can now continue. Why is this code inefficient? (what does the parent end up spending its time doing, particularly if the child thread takes a long time to complete?)

The parent spends a lot of CPU time in the loop doing nothing and wasting resources. It'd be better to use something like semaphores.

7. Now run helgrind on this program. What does it report? Is the code correct?

helgrind thinks that there's a data race in place, which is kinda weird. It also thinks that there's a data race around the printf function, possibly because the order isn't consistent across runs. The data race around done is wrong, the code is technically correct. The data race around the printf function is also a false positive.

8. Now look at a slightly modified version of the code, which is found in main-signal-cv.c. This version uses a condition variable to do the signaling (and associated lock). Why is this code preferred to the previous version? Is it correctness, or performance, or both?

`main-signal-cv` is strictly better. There are performance reasons: We don't waste cpu cycles (THEY DON'T GROW IN TREES, PEOPLE). Also because of correctness. And it makes helgrind happy.

9. Once again run helgrind on main-signal-cv. Does it report any errors?

Nop, helgrind is happy as a clown with `main-signal-cv`.
