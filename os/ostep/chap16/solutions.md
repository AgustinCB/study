1. First let’s use a tiny address space to translate some addresses. Here’s a simple set of parameters with a few different random seeds; can you translate the addresses?

```
$ ./segmentation.py -a 128 -p 512 -b 0 -l 20 -B 512 -L 20 -s 0

Segment register information:

  Segment 0 base  (grows positive) : 0x00000000 (decimal 0)
  Segment 0 limit                  : 20

  Segment 1 base  (grows negative) : 0x00000200 (decimal 512)
  Segment 1 limit                  : 20

Virtual Address Trace
  VA  0: 0x0000006c (decimal:  108) --> (Stack) 492
  VA  1: 0x00000061 (decimal:   97) --> (Stack) Violation
  VA  2: 0x00000035 (decimal:   53) --> (Heap) Violation
  VA  3: 0x00000021 (decimal:   33) --> (Heap) Violation
  VA  4: 0x00000041 (decimal:   65) --> (Stack) violation
```

```
segmentation.py -a 128 -p 512 -b 0 -l 20 -B 512 -L 20 -s 1

Segment register information:

  Segment 0 base  (grows positive) : 0x00000000 (decimal 0)
  Segment 0 limit                  : 20

  Segment 1 base  (grows negative) : 0x00000200 (decimal 512)
  Segment 1 limit                  : 20

Virtual Address Trace
  VA  0: 0x00000011 (decimal:   17) --> (Heap) 17
  VA  1: 0x0000006c (decimal:  108) --> (Stack) 492
  VA  2: 0x00000061 (decimal:   97) --> (Stack) Violation
  VA  3: 0x00000020 (decimal:   32) --> (Heap) Violation
  VA  4: 0x0000003f (decimal:   63) --> (Heap) Violation
```

```
$ ./segmentation.py -a 128 -p 512 -b 0 -l 20 -B 512 -L 20 -s 3

Segment register information:

  Segment 0 base  (grows positive) : 0x00000000 (decimal 0)
  Segment 0 limit                  : 20

  Segment 1 base  (grows negative) : 0x00000200 (decimal 512)
  Segment 1 limit                  : 20

Virtual Address Trace
  VA  0: 0x0000001e (decimal:   30) --> (Heap) Violation
  VA  1: 0x00000045 (decimal:   69) --> (Stack) Violation
  VA  2: 0x0000002f (decimal:   47) --> (Heap) Violation
  VA  3: 0x0000004d (decimal:   77) --> (Stack) Violation
  VA  4: 0x00000050 (decimal:   80) --> (Stack) Violation
```

2. Now, let’s see if we understand this tiny address space we’ve constructed (using the parameters from the question above). What is the highest legal virtual address in segment 0? What about the lowest legal virtual address in segment 1? What are the lowest and highest illegal addresses in this entire address space? Finally, how would you run segmentation.py with the -A flag to test if you are right?

Highest in segment 0: 19.
Lowest in segment 1: 108
Lowest illegal address: 21
Highest illegal address: 107

I'd test it using: 

`./segmentation.py -a 128 -p 512 -b 0 -l 20 -B 512 -L 20 -c -A 19,108,21,107`. This should cause the first two to pass and the next two to fail. By increasing and decreasing by one each one, it'd be possible to probe the boundaries.

3. Let’s say we have a tiny 16-byte address space in a 128-byte physical memory. What base and bounds would you set up so as to get the simulator to generate the following translation results for the specified address stream: valid, valid, violation, ..., violation, valid, valid? Assume the following parameters: `./segmentation.py -a 16 -p 128 -A 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15 --b0 ? --l0 ? --b1 ? --l1 ?`

I'd run:

`./segmentation.py -a 16 -p 128 -A 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15 -b 0 -l 2 -B 128 -L 2 -c`

4. Assuming we want to generate a problem where roughly 90% of the randomly-generated virtual addresses are valid (i.e., not segmentation violations). How should you configure the simulator to do so? Which parameters are important?

The important paramenters are the size of the address space, the bases and the limits. I'd configure it so that they follow this equation:

naddresses(heap_limit, stack_limit, heap_base, stack_base) =
	heap_limit + stack_limit   if (heap_base + heap_limit) - (stack_base - stack_limit) <= 0
       	heap_limit + stack_limit - ((heap_base + heap_limit) - (stack_base - stack_limit))   otherwise
0.9 = naddress(heap_limit, stack_limit, heap_base, stack_base) / address_space

5. Can you run the simulator such that no virtual addresses are valid? How?

Of course! Putting the limits to zero.
