1. First run with the flags -n 10 -H 0 -p BEST -s 0 to generate a few random allocations and frees. Can you predict what alloc()/free() will return? Can you guess the state of the free list after each request? What do you notice about the free list over time?

The free list tend to fragment over time.

```
ptr[0] = Alloc(3)  returned addr:1000, searches one.
List [ addr:1003, size: 97 ]

Free(ptr[0]) returned 0
List [ addr:1000, size: 3 ] [ addr:1003, size:97 ]

ptr[1] = Alloc(5)  returned addr:1003, searches two
List [ addr:1000, size: 3 ] [ addr:1008, size:92 ]

Free(ptr[1]) returned addr 9
List [ addr:1000, size: 3 ] [ addr:1003, size:5 ] [ addr:1008, size:92 ]

ptr[2] = Alloc(8)  returned addr:1008, searches three
List [ addr:1000, size: 3 ] [ addr:1003, size:5 ] [ addr:1016, size:84 ]

Free(ptr[2]) returned 0
List [ addr:1000, size: 3 ] [ addr:1003, size:5 ] [ addr:1008, size:8 ] [ addr:1016, size:84 ]

ptr[3] = Alloc(8)  returned addr:1008, searches four
List [ addr:1000, size: 3 ] [ addr:1003, size:5 ] [ addr:1016, size:84 ]

Free(ptr[3]) returned 0
List [ addr:1000, size: 3 ] [ addr:1003, size:5 ] [ addr:1008, size:8 ] [ addr:1016, size:84 ]

ptr[4] = Alloc(2)  returned addr: 1000, searches four
List [ addr:1002, size: 1 ] [ addr:1003, size:5 ] [ addr:1008, size:8 ] [ addr:1016, size:84 ]

ptr[5] = Alloc(7)  returned addr: 1008, searches four
List [ addr:1002, size: 1 ] [ addr:1003, size:5 ] [ addr:1015, size:1 ] [ addr:1016, size:84 ]
```

2. How are the results different when using a WORST fit policy to search the free list (-p WORST)? What changes?

```
ptr[0] = Alloc(3)  returned addr:1000, searches one.
List [ addr:1003, size: 97 ]

Free(ptr[0]) returned 0
List [ addr:1000, size: 3 ] [ addr:1003, size:97 ]

ptr[1] = Alloc(5)  returned addr:1003, searches two
List [ addr:1000, size: 3 ] [ addr:1008, size:92 ]

Free(ptr[1]) returned addr 9
List [ addr:1000, size: 3 ] [ addr:1003, size:5 ] [ addr:1008, size:92 ]

ptr[2] = Alloc(8)  returned addr:1008, searches three
List [ addr:1000, size: 3 ] [ addr:1003, size:5 ] [ addr:1016, size:84 ]

Free(ptr[2]) returned 0
List [ addr:1000, size: 3 ] [ addr:1003, size:5 ] [ addr:1008, size:8 ] [ addr:1016, size:84 ]

ptr[3] = Alloc(8)  returned addr:1016, searches four
List [ addr:1000, size: 3 ] [ addr:1003, size:5 ] [ addr:1008, size:8 ] [ addr:1024, size:76 ]

Free(ptr[3]) returned 0
List [ addr:1000, size: 3 ] [ addr:1003, size:5 ] [ addr:1008, size:8 ] [ addr:1016, size: 8 ] [ addr:1024, size:76 ]

ptr[4] = Alloc(2)  returned addr: 1024, searches five
List [ addr:1000, size: 3 ] [ addr:1003, size:5 ] [ addr:1008, size:8 ] [ addr:1016, size: 8 ] [ addr:1026, size:74 ]

ptr[5] = Alloc(7)  returned addr: 1026, searches five
List [ addr:1000, size: 3 ] [ addr:1003, size:5 ] [ addr:1008, size:8 ] [ addr:1016, size: 8 ] [ addr:1033, size:67 ]
```

The free list is even more fragmented.

3. What about when using FIRST fit (-p FIRST)? What speeds up when you use first fit?

```
ptr[0] = Alloc(3)  returned addr:1000, searches one.
List [ addr:1003, size: 97 ]

Free(ptr[0]) returned 0
List [ addr:1000, size: 3 ] [ addr:1003, size:97 ]

ptr[1] = Alloc(5)  returned addr:1003, searches two
List [ addr:1000, size: 3 ] [ addr:1008, size:92 ]

Free(ptr[1]) returned addr 9
List [ addr:1000, size: 3 ] [ addr:1003, size:5 ] [ addr:1008, size:92 ]

ptr[2] = Alloc(8)  returned addr:1008, searches three
List [ addr:1000, size: 3 ] [ addr:1003, size:5 ] [ addr:1016, size:84 ]

Free(ptr[2]) returned 0
List [ addr:1000, size: 3 ] [ addr:1003, size:5 ] [ addr:1008, size:8 ] [ addr:1016, size:84 ]

ptr[3] = Alloc(8)  returned addr:1008, searches three
List [ addr:1000, size: 3 ] [ addr:1003, size:5 ] [ addr:1016, size:84 ]

Free(ptr[3]) returned 0
List [ addr:1000, size: 3 ] [ addr:1003, size:5 ] [ addr:1008, size:8 ] [ addr:1016, size:84 ]

ptr[4] = Alloc(2)  returned addr: 1000, searches one
List [ addr:1002, size: 1 ] [ addr:1003, size:5 ] [ addr:1008, size:8 ] [ addr:1016, size:84 ]

ptr[5] = Alloc(7)  returned addr: 1008, searches three
List [ addr:1002, size: 1 ] [ addr:1003, size:5 ] [ addr:1015, size:1 ] [ addr:1016, size:84 ]
```

It's very similar to BEST, but now the searches are faster.

4. For the above questions, how the list is kept ordered can affect the time it takes to find a free location for some of the policies. Use the different free list orderings (-l ADDRSORT, -l SIZESORT+, -l SIZESORT-) to see how the policies and the list orderings interact.

ADDRSORT and SIZESORT+ have similar performance. However, SIZESORT- has a much better performance: All the seaches take one search. That being said, the free list gets more fragmented on this case.

5. Coalescing of a free list can be quite important. Increase the number of random allocations (say to -n 1000). What happens to larger allocation requests over time? Run with and without coalescing (i.e., without and with the -C flag). What differences in outcome do you see? How big is the free list over time in each case? Does the ordering of the list matter in this case?

Using ADDRSORT:

Without coalescing, the free list ends with 51 items. Which is far too much. With it it finishes with one item! The performance is far better.

Using SIZESORT+:

Without coalescing, the free list ends with 31 items. Which is far too much. With it it finishes with 28 items. AThere isn't a big difference.

Using SIZESORT-:

Without coalescing, the free list ends with 100 items. Which is far too much. With it it finishes with 98 items. There isn't a big difference. And that shows how much fragmentation happens with SIZESORT-: At the end it wasn't even able to allocate some new memory because it was too fragmented.

6. What happens when you change the percent allocated fraction -P to higher than 50? What happens to allocations as it nears 100? What about as it nears 0?

It runs out of usable memory faster as you increase the percentage of allocations. Even coalescing causes some -1 when trying to allocate memory.

With very low percentage of allocations, the number of -1 returned tends to zero.

7. What kind of specific requests can you make to generate a highlyfragmented free space? Use the -A flag to create fragmented free lists, and see how different policies and options change the organization of the free list.

The most obvious way to fragment the memory would be start requesting a high number of memory, free it, request an slightly smaller memory, free it and repeat. That way you can eventually have free items of one unit size which become pretty much unusuable.

This, for example, would end up with a free list of ten items, all of size one:

```
./malloc.py -H 0 -A +9,-0,+8,-1,+7,-2,+6,-3,+5,-4,+4,-5,+3,-6,+2,-7,+1,-8 -S 10 -p FIRST -s 0 -c -l ADDRSORT
```

That problem could be solved using coalesce in the case of the list being ordered using either ADDRSORT or SIZESORT-. For SIZESORT+ is still pretty bad: It ends with a list of size 9. In this case, the police doesn't make a difference.

If you do coalesce, you could first alloc a bunch of small chunks and then free every second one. For example:

```
./malloc.py -H 0 -A +1,+1,+1,+1,+1,+1,+1,+1,+1,+1,-0,-2,-4,-6,-8 -S 10 -p FIRST -s 0 -c -l ADDRSORT -C
```

This will end up with free list of size 5, but with each element of size 1. It will have such an effect no matter how you order. In this case, the police doesn't make a difference.
