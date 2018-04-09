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
