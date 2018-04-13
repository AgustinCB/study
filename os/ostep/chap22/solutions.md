1. Generate random addresses with the following arguments: -s 0 -n 10, -s 1 -n 10, and -s 2 -n 10. Change the policy from FIFO, to LRU, to OPT. Compute whether each access in said address traces are hits or misses.

```
./paging-policy.py -s 0 -n 10 -a -1 -p FIFO
ARG addresses -1
ARG addressfile 
ARG numaddrs 10
ARG policy FIFO
ARG clockbits 2
ARG cachesize 3
ARG maxpage 10
ARG seed 0
ARG notrace False

Assuming a replacement policy of FIFO, and a cache of size 3 pages,
figure out whether each of the following page references hit or miss
in the page cache.

Access: 8  Hit/Miss?  State of Memory? -> Miss, memory [8], lastin replaced: -
Access: 7  Hit/Miss?  State of Memory? -> Miss, memory [8,7], lastin replaced: -
Access: 4  Hit/Miss?  State of Memory? -> Miss, memory [8,7,4], lastin replaced: -
Access: 2  Hit/Miss?  State of Memory? -> Miss, memory [7,4,2], lastin replaced: 8
Access: 5  Hit/Miss?  State of Memory? -> Miss, memory [4,2,5], lastin replaced: 7
Access: 4  Hit/Miss?  State of Memory? -> Hit, memory [4,2,5], lastin replaced: -
Access: 7  Hit/Miss?  State of Memory? -> Miss, memory [2,5,7], lastin replaced: 4
Access: 3  Hit/Miss?  State of Memory? -> Miss, memory [5,7,3], lastin replaced: 2
Access: 4  Hit/Miss?  State of Memory? -> Miss, memory [7,3,4], lastin replaced: 5
Access: 5  Hit/Miss?  State of Memory? -> Miss, memory [3,4,5], lastin replaced: 7

./paging-policy.py -s 1 -n 10 -a -1 -p FIFO
ARG addresses -1
ARG addressfile 
ARG numaddrs 10
ARG policy FIFO
ARG clockbits 2
ARG cachesize 3
ARG maxpage 10
ARG seed 1
ARG notrace False

Assuming a replacement policy of FIFO, and a cache of size 3 pages,
figure out whether each of the following page references hit or miss
in the page cache.

Access: 1  Hit/Miss?  State of Memory? -> Miss, memory [1], lastin replaced: -
Access: 8  Hit/Miss?  State of Memory? -> Miss, memory [1,8], lastin replaced: -
Access: 7  Hit/Miss?  State of Memory? -> Miss, memory [1,8,7], lastin replaced: -
Access: 2  Hit/Miss?  State of Memory? -> Miss, memory [8,7,2], lastin replaced: 1
Access: 4  Hit/Miss?  State of Memory? -> Miss, memory [7,2,4], lastin replaced: 8
Access: 4  Hit/Miss?  State of Memory? -> Hit, memory [7,2,4], lastin replaced: -
Access: 6  Hit/Miss?  State of Memory? -> Miss, memory [2,4,6], lastin replaced: 7
Access: 7  Hit/Miss?  State of Memory? -> Miss, memory [4,6,7], lastin replaced: 2
Access: 0  Hit/Miss?  State of Memory? -> Miss, memory [6,7,0], lastin replaced: 4
Access: 0  Hit/Miss?  State of Memory? -> Hit, memory [6,7,0], lastin replaced: -

./paging-policy.py -s 2 -n 10 -a -1 -p FIFO
ARG addresses -1
ARG addressfile 
ARG numaddrs 10
ARG policy FIFO
ARG clockbits 2
ARG cachesize 3
ARG maxpage 10
ARG seed 2
ARG notrace False

Assuming a replacement policy of FIFO, and a cache of size 3 pages,
figure out whether each of the following page references hit or miss
in the page cache.

Access: 9  Hit/Miss?  State of Memory? -> Miss, memory [9], lastin replaced: -
Access: 9  Hit/Miss?  State of Memory? -> Hit, memory [9], lastin replaced: -
Access: 0  Hit/Miss?  State of Memory? -> Miss, memory [9,0], lastin replaced: -
Access: 0  Hit/Miss?  State of Memory? -> Hit, memory [9,0], lastin replaced: -
Access: 8  Hit/Miss?  State of Memory? -> Miss, memory [9,0,8], lastin replaced: -
Access: 7  Hit/Miss?  State of Memory? -> Miss, memory [0,8,7], lastin replaced: 9
Access: 6  Hit/Miss?  State of Memory? -> Miss, memory [8,7,6], lastin replaced: 0
Access: 3  Hit/Miss?  State of Memory? -> Miss, memory [7,6,3], lastin replaced: 8
Access: 6  Hit/Miss?  State of Memory? -> Hit, memory [7,6,3], lastin replaced: -
Access: 6  Hit/Miss?  State of Memory? -> Hit, memory [7,6,3], lastin replaced: -

./paging-policy.py -s 0 -n 10 -a -1 -p LRU
ARG addresses -1
ARG addressfile 
ARG numaddrs 10
ARG policy LRU
ARG clockbits 2
ARG cachesize 3
ARG maxpage 10
ARG seed 0
ARG notrace False

Assuming a replacement policy of LRU, and a cache of size 3 pages,
figure out whether each of the following page references hit or miss
in the page cache.

Access: 8  Hit/Miss?  State of Memory? -> Miss, memory [6], lastin replaced: -
Access: 7  Hit/Miss?  State of Memory? -> Miss, memory [6,7], lastin replaced: -
Access: 4  Hit/Miss?  State of Memory? -> Miss, memory [6,7,4], lastin replaced: -
Access: 2  Hit/Miss?  State of Memory? -> Miss, memory [7,4,2], lastin replaced: 6
Access: 5  Hit/Miss?  State of Memory? -> Miss, memory [4,2,5], lastin replaced: 7
Access: 4  Hit/Miss?  State of Memory? -> Hit, memory [2,5,4], lastin replaced: -
Access: 7  Hit/Miss?  State of Memory? -> Miss, memory [5,4,7], lastin replaced: 2
Access: 3  Hit/Miss?  State of Memory? -> Miss, memory [4,7,3], lastin replaced: 5
Access: 4  Hit/Miss?  State of Memory? -> Hit, memory [7,3,4], lastin replaced: -
Access: 5  Hit/Miss?  State of Memory? -> Miss, memory [3,4,5], lastin replaced: 7

./paging-policy.py -s 1 -n 10 -a -1 -p LRU
ARG addresses -1
ARG addressfile 
ARG numaddrs 10
ARG policy LRU
ARG clockbits 2
ARG cachesize 3
ARG maxpage 10
ARG seed 1
ARG notrace False

Assuming a replacement policy of LRU, and a cache of size 3 pages,
figure out whether each of the following page references hit or miss
in the page cache.

Access: 1  Hit/Miss?  State of Memory? <- Miss, memory [1], lastin replaced: -
Access: 8  Hit/Miss?  State of Memory? <- Miss, memory [1,8], lastin replaced: -
Access: 7  Hit/Miss?  State of Memory? <- Miss, memory [1,8,7], lastin replaced: -
Access: 2  Hit/Miss?  State of Memory? <- Miss, memory [8,7,2], lastin replaced: 1
Access: 4  Hit/Miss?  State of Memory? <- Miss, memory [7,2,4], lastin replaced: 8
Access: 4  Hit/Miss?  State of Memory? <- Hit, memory [7,2,4], lastin replaced: -
Access: 6  Hit/Miss?  State of Memory? <- Miss, memory [2,4,6], lastin replaced: 7
Access: 7  Hit/Miss?  State of Memory? <- Miss, memory [4,6,7], lastin replaced: 2
Access: 0  Hit/Miss?  State of Memory? <- Miss, memory [6,7,0], lastin replaced: 4
Access: 0  Hit/Miss?  State of Memory? <- Hit, memory [6,7,0], lastin replaced: -

$ ./paging-policy.py -s 2 -n 10 -a -1 -p LRU
ARG addresses -1
ARG addressfile 
ARG numaddrs 10
ARG policy LRU
ARG clockbits 2
ARG cachesize 3
ARG maxpage 10
ARG seed 2
ARG notrace False

Assuming a replacement policy of LRU, and a cache of size 3 pages,
figure out whether each of the following page references hit or miss
in the page cache.

Access: 9  Hit/Miss?  State of Memory? <- Miss, memory [9], lastin replaced: -
Access: 9  Hit/Miss?  State of Memory? <- Hit, memory [9], lastin replaced: -
Access: 0  Hit/Miss?  State of Memory? <- Miss, memory [9,0], lastin replaced: -
Access: 0  Hit/Miss?  State of Memory? <- Hit, memory [9,0], lastin replaced: -
Access: 8  Hit/Miss?  State of Memory? <- Miss, memory [9,0,8], lastin replaced: -
Access: 7  Hit/Miss?  State of Memory? <- Miss, memory [0,8,7], lastin replaced: 9
Access: 6  Hit/Miss?  State of Memory? <- Miss, memory [8,7,6], lastin replaced: 0
Access: 3  Hit/Miss?  State of Memory? <- Miss, memory [7,6,3], lastin replaced: 8
Access: 6  Hit/Miss?  State of Memory? <- Hit, memory [7,6,3], lastin replaced: -
Access: 6  Hit/Miss?  State of Memory? <- Hit, memory [7,6,3], lastin replaced: -

./paging-policy.py -s 0 -n 10 -a -1 -p OPT
ARG addresses -1
ARG addressfile 
ARG numaddrs 10
ARG policy OPT
ARG clockbits 2
ARG cachesize 3
ARG maxpage 10
ARG seed 0
ARG notrace False

Assuming a replacement policy of OPT, and a cache of size 3 pages,
figure out whether each of the following page references hit or miss
in the page cache.

Access: 8  Hit/Miss?  State of Memory? -> Miss, memory [8], Right replaced: -
Access: 7  Hit/Miss?  State of Memory? -> Miss, memory [8,7], Right replaced: -
Access: 4  Hit/Miss?  State of Memory? -> Miss, memory [8,7,4], Right replaced: -
Access: 2  Hit/Miss?  State of Memory? -> Miss, memory [7,4,2], Right replaced: 8
Access: 5  Hit/Miss?  State of Memory? -> Miss, memory [7,4,5], Right replaced: 2
Access: 4  Hit/Miss?  State of Memory? -> Hit, memory [7,4,5], Right replaced: -
Access: 7  Hit/Miss?  State of Memory? -> Hit, memory [7,4,5], Right replaced: -
Access: 3  Hit/Miss?  State of Memory? -> Miss, memory [4,5,3], Right replaced: 7
Access: 4  Hit/Miss?  State of Memory? -> Hit, memory [4,5,3], Right replaced: -
Access: 5  Hit/Miss?  State of Memory? -> Hit, memory [4,5,3], Right replaced: -

./paging-policy.py -s 1 -n 10 -a -1 -p OPT
ARG addresses -1
ARG addressfile 
ARG numaddrs 10
ARG policy OPT
ARG clockbits 2
ARG cachesize 3
ARG maxpage 10
ARG seed 1
ARG notrace False

Assuming a replacement policy of OPT, and a cache of size 3 pages,
figure out whether each of the following page references hit or miss
in the page cache.

Access: 1  Hit/Miss?  State of Memory? -> Miss, memory [1], Right replaced: -
Access: 8  Hit/Miss?  State of Memory? -> Miss, memory [1,8], Right replaced: -
Access: 7  Hit/Miss?  State of Memory? -> Miss, memory [1,8,7], Right replaced: -
Access: 2  Hit/Miss?  State of Memory? -> Miss, memory [8,7,2], Right replaced: 1
Access: 4  Hit/Miss?  State of Memory? -> Miss, memory [8,7,4], Right replaced: 2
Access: 4  Hit/Miss?  State of Memory? -> Hit, memory [8,7,4], Right replaced: -
Access: 6  Hit/Miss?  State of Memory? -> Miss, memory [7,4,6], Right replaced: 8
Access: 7  Hit/Miss?  State of Memory? -> Hit, memory [7,4,6], Right replaced: -
Access: 0  Hit/Miss?  State of Memory? -> Miss, memory [4,6,0], Right replaced: 7
Access: 0  Hit/Miss?  State of Memory? -> Hit, memory [4,6,0], Right replaced: -

$ ./paging-policy.py -s 2 -n 10 -a -1 -p OPT
ARG addresses -1
ARG addressfile 
ARG numaddrs 10
ARG policy OPT
ARG clockbits 2
ARG cachesize 3
ARG maxpage 10
ARG seed 2
ARG notrace False

Assuming a replacement policy of OPT, and a cache of size 3 pages,
figure out whether each of the following page references hit or miss
in the page cache.

Access: 9  Hit/Miss?  State of Memory? -> Miss, memory [9], Right replaced: -
Access: 9  Hit/Miss?  State of Memory? -> Hit, memory [9], Right replaced: -
Access: 0  Hit/Miss?  State of Memory? -> Miss, memory [9,0], Right replaced: -
Access: 0  Hit/Miss?  State of Memory? -> Hit, memory [9,0], Right replaced: -
Access: 8  Hit/Miss?  State of Memory? -> Miss, memory [9,0,8], Right replaced: -
Access: 7  Hit/Miss?  State of Memory? -> Miss, memory [0,8,7], Right replaced: 9
Access: 6  Hit/Miss?  State of Memory? -> Miss, memory [8,7,6], Right replaced: 0
Access: 3  Hit/Miss?  State of Memory? -> Miss, memory [7,6,3], Right replaced: 7
Access: 6  Hit/Miss?  State of Memory? -> Hit, memory [7,6,3], Right replaced: -
Access: 6  Hit/Miss?  State of Memory? -> Hit, memory [7,6,3], Right replaced: -
```

2. For a cache of size 5, generate worst-case address reference streams for each of the following policies: FIFO, LRU, and MRU (worst-case reference streams cause the most misses possible. For the worst case reference streams, how much bigger of a cache is needed to improve performance dramatically and approach OPT?

Doing it for FIFO is relatively easy: Generate an stream from 1 to C+1 where C is the cache size:

```
./paging-policy.py --cachesize=5 -a 1,2,3,4,5,6,1,2,3,4,5,6 -p FIFO -c
ARG addresses 1,2,3,4,5,6,1,2,3,4,5,6
ARG addressfile 
ARG numaddrs 10
ARG policy FIFO
ARG clockbits 2
ARG cachesize 5
ARG maxpage 10
ARG seed 0
ARG notrace False

Solving...

Access: 1  MISS FirstIn ->          [1] <- Lastin  Replaced:- [Hits:0 Misses:1]
Access: 2  MISS FirstIn ->       [1, 2] <- Lastin  Replaced:- [Hits:0 Misses:2]
Access: 3  MISS FirstIn ->    [1, 2, 3] <- Lastin  Replaced:- [Hits:0 Misses:3]
Access: 4  MISS FirstIn -> [1, 2, 3, 4] <- Lastin  Replaced:- [Hits:0 Misses:4]
Access: 5  MISS FirstIn -> [1, 2, 3, 4, 5] <- Lastin  Replaced:- [Hits:0 Misses:5]
Access: 6  MISS FirstIn -> [2, 3, 4, 5, 6] <- Lastin  Replaced:1 [Hits:0 Misses:6]
Access: 1  MISS FirstIn -> [3, 4, 5, 6, 1] <- Lastin  Replaced:2 [Hits:0 Misses:7]
Access: 2  MISS FirstIn -> [4, 5, 6, 1, 2] <- Lastin  Replaced:3 [Hits:0 Misses:8]
Access: 3  MISS FirstIn -> [5, 6, 1, 2, 3] <- Lastin  Replaced:4 [Hits:0 Misses:9]
Access: 4  MISS FirstIn -> [6, 1, 2, 3, 4] <- Lastin  Replaced:5 [Hits:0 Misses:10]
Access: 5  MISS FirstIn -> [1, 2, 3, 4, 5] <- Lastin  Replaced:6 [Hits:0 Misses:11]
Access: 6  MISS FirstIn -> [2, 3, 4, 5, 6] <- Lastin  Replaced:1 [Hits:0 Misses:12]

FINALSTATS hits 0   misses 12   hitrate 0.00
```

You can do something similar with LRU and it will give the same results, since the difference between the two methods appears only in hits. For MRU you have to generate C+1 different address, and then change from C to C+1:

```
./paging-policy.py --cachesize=5 -a 1,2,3,4,5,6,5,6,5,6 -p MRU -c
ARG addresses 1,2,3,4,5,6,5,6,5,6
ARG addressfile 
ARG numaddrs 10
ARG policy MRU
ARG clockbits 2
ARG cachesize 5
ARG maxpage 10
ARG seed 0
ARG notrace False

Solving...

Access: 1  MISS LRU ->          [1] <- MRU Replaced:- [Hits:0 Misses:1]
Access: 2  MISS LRU ->       [1, 2] <- MRU Replaced:- [Hits:0 Misses:2]
Access: 3  MISS LRU ->    [1, 2, 3] <- MRU Replaced:- [Hits:0 Misses:3]
Access: 4  MISS LRU -> [1, 2, 3, 4] <- MRU Replaced:- [Hits:0 Misses:4]
Access: 5  MISS LRU -> [1, 2, 3, 4, 5] <- MRU Replaced:- [Hits:0 Misses:5]
Access: 6  MISS LRU -> [1, 2, 3, 4, 6] <- MRU Replaced:5 [Hits:0 Misses:6]
Access: 5  MISS LRU -> [1, 2, 3, 4, 5] <- MRU Replaced:6 [Hits:0 Misses:7]
Access: 6  MISS LRU -> [1, 2, 3, 4, 6] <- MRU Replaced:5 [Hits:0 Misses:8]
Access: 5  MISS LRU -> [1, 2, 3, 4, 5] <- MRU Replaced:6 [Hits:0 Misses:9]
Access: 6  MISS LRU -> [1, 2, 3, 4, 6] <- MRU Replaced:5 [Hits:0 Misses:10]

FINALSTATS hits 0   misses 10   hitrate 0.00
```

In all cases, increasing it by one will make them approach OPT.

3. Generate a random trace (use python or perl). How would you expect the different policies to perform on such a trace?

The trace generated is: 3, 6, 1, 3, 1, 4, 8, 9, 10, 6.

With that trace, FIFO and LRU will need at least a three sized cache to start having hits. And a seven sized cache to have the biggest amount of hits. For OPT it will be enough with two.

For MRU, the first with two you will already have two out of the three possible hits. And you will need seven to have the maximum.

4. Now generate a trace with some locality. How can you generate such a trace? How does LRU perform on it? How much better than RAND is LRU? How does CLOCK do? How about CLOCK with different numbers of clock bits?

I generated it this way:

```
>>> l = [1,1,1,2,2,2,3,3,3,4,5,6,7,8,9]
>>> from random import *
>>> [choice(l) for i in range(10)]
[2, 3, 7, 3, 1, 2, 6, 1, 2, 8]
```

With a cache size of three, LRU has a fairly good performance: 30% hitrate. The hitrate of RAND goes between 10-30%.

CLOCK has a 20% hitrae. It doesn't vary with the number of clock bits.

5. Use a program like valgrind to instrument a real application and generate a virtual page reference stream. For example, running valgrind --tool=lackey --trace-mem=yes ls will output a nearly-complete reference trace of every instruction and data reference made by the program ls. To make this useful for the simulator above, you’ll have to first transform each virtual memory reference into a virtual page-number reference (done by masking off the offset and shifting the resulting bits downward). How big of a cache is needed for your application trace in order to satisfy a large fraction of requests? Plot a graph of its working set as the size of the cache increases.

