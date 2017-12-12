1. Before doing any translations, let’s use the simulator to study how linear page tables change size given different parameters. Compute the size of linear page tables as different parameters change. Some suggested inputs are below; by using the -v flag, you can see how many page-table entries are filled. First, to understand how linear page table size changes as the address
space grows:

paging-linear-translate.py -P 1k -a 1m -p 512m -v -n 0
paging-linear-translate.py -P 1k -a 2m -p 512m -v -n 0
paging-linear-translate.py -P 1k -a 4m -p 512m -v -n 0

Then, to understand how linear page table size changes as page size
grows:

paging-linear-translate.py -P 1k -a 1m -p 512m -v -n 0
paging-linear-translate.py -P 2k -a 1m -p 512m -v -n 0
paging-linear-translate.py -P 4k -a 1m -p 512m -v -n 0

Before running any of these, try to think about the expected trends. How should page-table size change as the address space grows? As the page size grows? Why shouldn’t we just use really big pages in general?

-------------------------------------------------------------------------------

The page-table size would grow as the address space grows and shrink as the page size grows.

We should avoid using big page tables because they occupy space in memory.

2. Now let’s do some translations. Start with some small examples, and change the number of pages that are allocated to the address space with the -u flag. For example:

paging-linear-translate.py -P 1k -a 16k -p 32k -v -u 0
paging-linear-translate.py -P 1k -a 16k -p 32k -v -u 25
paging-linear-translate.py -P 1k -a 16k -p 32k -v -u 50
paging-linear-translate.py -P 1k -a 16k -p 32k -v -u 75
paging-linear-translate.py -P 1k -a 16k -p 32k -v -u 100

What happens as you increase the percentage of pages that are allocated in each address space?

-------------------------------------------------------------------------------

```
$ ./paging-linear-translate.py -P 1k -a 16k -p 32k -v -u 0
ARG seed 0
ARG address space size 16k
ARG phys mem size 32k
ARG page size 1k
ARG verbose True
ARG addresses -1


The format of the page table is simple:
The high-order (left-most) bit is the VALID bit.
  If the bit is 1, the rest of the entry is the PFN.
  If the bit is 0, the page is not valid.
Use verbose mode (-v) if you want to print the VPN # by
each entry of the page table.

Page Table (from entry 0 down to the max size)
  [       0]   0x00000000
  [       1]   0x00000000
  [       2]   0x00000000
  [       3]   0x00000000
  [       4]   0x00000000
  [       5]   0x00000000
  [       6]   0x00000000
  [       7]   0x00000000
  [       8]   0x00000000
  [       9]   0x00000000
  [      10]   0x00000000
  [      11]   0x00000000
  [      12]   0x00000000
  [      13]   0x00000000
  [      14]   0x00000000
  [      15]   0x00000000

Virtual Address Trace
  VA 0x00003a39 (decimal:    14905) --> invalid
  VA 0x00003ee5 (decimal:    16101) --> invalid
  VA 0x000033da (decimal:    13274) --> invalid
  VA 0x000039bd (decimal:    14781) --> invalid
  VA 0x000013d9 (decimal:     5081) --> invalid

For each virtual address, write down the physical address it translates to
OR write down that it is an out-of-bounds address (e.g., segfault).
```

```
./paging-linear-translate.py -P 1k -a 16k -p 32k -v -u 25
ARG seed 0
ARG address space size 16k
ARG phys mem size 32k
ARG page size 1k
ARG verbose True
ARG addresses -1


The format of the page table is simple:
The high-order (left-most) bit is the VALID bit.
  If the bit is 1, the rest of the entry is the PFN.
  If the bit is 0, the page is not valid.
Use verbose mode (-v) if you want to print the VPN # by
each entry of the page table.

Page Table (from entry 0 down to the max size)
  [       0]   0x80000018
  [       1]   0x00000000
  [       2]   0x00000000
  [       3]   0x00000000
  [       4]   0x00000000
  [       5]   0x80000009
  [       6]   0x00000000
  [       7]   0x00000000
  [       8]   0x80000010
  [       9]   0x00000000
  [      10]   0x80000013
  [      11]   0x00000000
  [      12]   0x8000001f
  [      13]   0x8000001c
  [      14]   0x00000000
  [      15]   0x00000000

Virtual Address Trace
  VA 0x00003986 (decimal:    14726) --> page 14 is invalid
  VA 0x00002bc6 (decimal:    11206) --> In page 10: 0x4fc6
  VA 0x00001e37 (decimal:     7735) --> page 7 is invalid
  VA 0x00000671 (decimal:     1649) --> page 1 is invalid
  VA 0x00001bc9 (decimal:     7113) --> page 6 is invalid

For each virtual address, write down the physical address it translates to
OR write down that it is an out-of-bounds address (e.g., segfault).
```

```
$ ./paging-linear-translate.py -P 1k -a 16k -p 32k -v -u 50
ARG seed 0
ARG address space size 16k
ARG phys mem size 32k
ARG page size 1k
ARG verbose True
ARG addresses -1


The format of the page table is simple:
The high-order (left-most) bit is the VALID bit.
  If the bit is 1, the rest of the entry is the PFN.
  If the bit is 0, the page is not valid.
Use verbose mode (-v) if you want to print the VPN # by
each entry of the page table.

Page Table (from entry 0 down to the max size)
  [       0]   0x80000018
  [       1]   0x00000000
  [       2]   0x00000000
  [       3]   0x8000000c
  [       4]   0x80000009
  [       5]   0x00000000
  [       6]   0x8000001d
  [       7]   0x80000013
  [       8]   0x00000000
  [       9]   0x8000001f
  [      10]   0x8000001c
  [      11]   0x00000000
  [      12]   0x8000000f
  [      13]   0x00000000
  [      14]   0x00000000
  [      15]   0x80000008

Virtual Address Trace
  VA 0x00003385 (decimal:    13189) --> In page 12: 0x3f85
  VA 0x0000231d (decimal:     8989) --> page 8 is invalid
  VA 0x000000e6 (decimal:      230) --> In page 0: 0x60e6
  VA 0x00002e0f (decimal:    11791) --> page 11 is invalid
  VA 0x00001986 (decimal:     6534) --> In page 6: 0x7586

For each virtual address, write down the physical address it translates to
OR write down that it is an out-of-bounds address (e.g., segfault).
```

```
$ ./paging-linear-translate.py -P 1k -a 16k -p 32k -v -u 75
ARG seed 0
ARG address space size 16k
ARG phys mem size 32k
ARG page size 1k
ARG verbose True
ARG addresses -1


The format of the page table is simple:
The high-order (left-most) bit is the VALID bit.
  If the bit is 1, the rest of the entry is the PFN.
  If the bit is 0, the page is not valid.
Use verbose mode (-v) if you want to print the VPN # by
each entry of the page table.

Page Table (from entry 0 down to the max size)
  [       0]   0x80000018
  [       1]   0x80000008
  [       2]   0x8000000c
  [       3]   0x80000009
  [       4]   0x80000012
  [       5]   0x80000010
  [       6]   0x8000001f
  [       7]   0x8000001c
  [       8]   0x80000017
  [       9]   0x80000015
  [      10]   0x80000003
  [      11]   0x80000013
  [      12]   0x8000001e
  [      13]   0x8000001b
  [      14]   0x80000019
  [      15]   0x80000000

Virtual Address Trace
  VA 0x00002e0f (decimal:    11791) --> In page 11: 0x4e0f
  VA 0x00001986 (decimal:     6534) --> In page 6: 0x7d86
  VA 0x000034ca (decimal:    13514) --> In page 13: 0x6cca
  VA 0x00002ac3 (decimal:    10947) --> In page 10: 0x0ec3
  VA 0x00000012 (decimal:       18) --> In page 0: 0x6012

For each virtual address, write down the physical address it translates to
OR write down that it is an out-of-bounds address (e.g., segfault).
```

```
$ ./paging-linear-translate.py -P 1k -a 16k -p 32k -v -u 100
ARG seed 0
ARG address space size 16k
ARG phys mem size 32k
ARG page size 1k
ARG verbose True
ARG addresses -1


The format of the page table is simple:
The high-order (left-most) bit is the VALID bit.
  If the bit is 1, the rest of the entry is the PFN.
  If the bit is 0, the page is not valid.
Use verbose mode (-v) if you want to print the VPN # by
each entry of the page table.

Page Table (from entry 0 down to the max size)
  [       0]   0x80000018
  [       1]   0x80000008
  [       2]   0x8000000c
  [       3]   0x80000009
  [       4]   0x80000012
  [       5]   0x80000010
  [       6]   0x8000001f
  [       7]   0x8000001c
  [       8]   0x80000017
  [       9]   0x80000015
  [      10]   0x80000003
  [      11]   0x80000013
  [      12]   0x8000001e
  [      13]   0x8000001b
  [      14]   0x80000019
  [      15]   0x80000000

Virtual Address Trace
  VA 0x00002e0f (decimal:    11791) --> In page 11: 0x4e0f
  VA 0x00001986 (decimal:     6534) --> In page 6: 0x7d86
  VA 0x000034ca (decimal:    13514) --> In page 13: 0x6cca
  VA 0x00002ac3 (decimal:    10947) --> In page 10: 0x0ec3
  VA 0x00000012 (decimal:       18) --> In page 0: 0x6012

For each virtual address, write down the physical address it translates to
OR write down that it is an out-of-bounds address (e.g., segfault).
```

3. Now let’s try some different random seeds, and some different (and sometimes quite crazy) address-space parameters, for variety:

paging-linear-translate.py -P 8 -a 32 -p 1024 -v -s 1
paging-linear-translate.py -P 8k -a 32k -p 1m -v -s 2
paging-linear-translate.py -P 1m -a 256m -p 512m -v -s 3

Which of these parameter combinations are unrealistic? Why?

The first one is very unrealistic for small.

The second one... It may make sense, yeah, in small devices with small amount of memory. Or really old machines.

The third one doesn't make sense because the page size is too big. There would be too much internal fragmentation.

```
$ ./paging-linear-translate.py -P 8 -a 32 -p 1024 -v -s 1
ARG seed 1
ARG address space size 32
ARG phys mem size 1024
ARG page size 8
ARG verbose True
ARG addresses -1


The format of the page table is simple:
The high-order (left-most) bit is the VALID bit.
  If the bit is 1, the rest of the entry is the PFN.
  If the bit is 0, the page is not valid.
Use verbose mode (-v) if you want to print the VPN # by
each entry of the page table.

Page Table (from entry 0 down to the max size)
  [       0]   0x00000000
  [       1]   0x80000061
  [       2]   0x00000000
  [       3]   0x00000000

Virtual Address Trace
  VA 0x0000000e (decimal:       14) --> In page 1: 0x30e
  VA 0x00000014 (decimal:       20) --> page 2 is invalid
  VA 0x00000019 (decimal:       25) --> page 3 is invalid
  VA 0x00000003 (decimal:        3) --> page 0 is invalid
  VA 0x00000000 (decimal:        0) --> page 0 is invalid

For each virtual address, write down the physical address it translates to
OR write down that it is an out-of-bounds address (e.g., segfault).
```

```
$ ./paging-linear-translate.py -P 8k -a 32k -p 1m -v -s 2
ARG seed 2
ARG address space size 32k
ARG phys mem size 1m
ARG page size 8k
ARG verbose True
ARG addresses -1


The format of the page table is simple:
The high-order (left-most) bit is the VALID bit.
  If the bit is 1, the rest of the entry is the PFN.
  If the bit is 0, the page is not valid.
Use verbose mode (-v) if you want to print the VPN # by
each entry of the page table.

Page Table (from entry 0 down to the max size)
  [       0]   0x80000079
  [       1]   0x00000000
  [       2]   0x00000000
  [       3]   0x8000005e

Virtual Address Trace
  VA 0x000055b9 (decimal:    21945) --> page 2 is invalid
  VA 0x00002771 (decimal:    10097) --> page 1 is invalid
  VA 0x00004d8f (decimal:    19855) --> page 2 is invalid
  VA 0x00004dab (decimal:    19883) --> page 2 is invalid
  VA 0x00004a64 (decimal:    19044) --> page 2 is invalid

For each virtual address, write down the physical address it translates to
OR write down that it is an out-of-bounds address (e.g., segfault).
```

```
$ ./paging-linear-translate.py -P 1m -a 256m -p 512m -v -s 3
ARG seed 3
ARG address space size 256m
ARG phys mem size 512m
ARG page size 1m
ARG verbose True
ARG addresses -1


The format of the page table is simple:
The high-order (left-most) bit is the VALID bit.
  If the bit is 1, the rest of the entry is the PFN.
  If the bit is 0, the page is not valid.
Use verbose mode (-v) if you want to print the VPN # by
each entry of the page table.

Page Table (from entry 0 down to the max size)
  [       0]   0x00000000
  [       1]   0x800000bd
  [       2]   0x80000140
  [       3]   0x00000000
  [       4]   0x00000000
  [       5]   0x80000084
  [       6]   0x00000000
  [       7]   0x800000f0
  [       8]   0x800000f3
  [       9]   0x8000004d
  [      10]   0x800001bc
  [      11]   0x8000017b
  [      12]   0x80000020
  [      13]   0x8000012e
  [      14]   0x00000000
  [      15]   0x00000000
  [      16]   0x800000f2
  [      17]   0x800001c1
  [      18]   0x800001d7
  [      19]   0x00000000
  [      20]   0x800000e3
  [      21]   0x00000000
  [      22]   0x00000000
  [      23]   0x00000000
  [      24]   0x800000df
  [      25]   0x8000009a
  [      26]   0x800000c5
  [      27]   0x00000000
  [      28]   0x8000012b
  [      29]   0x8000015d
  [      30]   0x800001b6
  [      31]   0x80000157
  [      32]   0x00000000
  [      33]   0x800001ed
  [      34]   0x80000123
  [      35]   0x8000006c
  [      36]   0x80000125
  [      37]   0x00000000
  [      38]   0x00000000
  [      39]   0x800001fa
  [      40]   0x00000000
  [      41]   0x800000d2
  [      42]   0x00000000
  [      43]   0x00000000
  [      44]   0x800001be
  [      45]   0x00000000
  [      46]   0x80000017
  [      47]   0x800000a9
  [      48]   0x800001f6
  [      49]   0x800001ff
  [      50]   0x00000000
  [      51]   0x00000000
  [      52]   0x80000010
  [      53]   0x00000000
  [      54]   0x00000000
  [      55]   0x8000004f
  [      56]   0x00000000
  [      57]   0x800000a0
  [      58]   0x800001cb
  [      59]   0x00000000
  [      60]   0x00000000
  [      61]   0x80000149
  [      62]   0x8000011e
  [      63]   0x800001e1
  [      64]   0x800000dc
  [      65]   0x80000079
  [      66]   0x00000000
  [      67]   0x8000010a
  [      68]   0x80000005
  [      69]   0x00000000
  [      70]   0x8000000a
  [      71]   0x80000143
  [      72]   0x00000000
  [      73]   0x800000ee
  [      74]   0x800000b4
  [      75]   0x80000179
  [      76]   0x00000000
  [      77]   0x00000000
  [      78]   0x00000000
  [      79]   0x00000000
  [      80]   0x800000a3
  [      81]   0x00000000
  [      82]   0x00000000
  [      83]   0x00000000
  [      84]   0x80000099
  [      85]   0x00000000
  [      86]   0x8000000d
  [      87]   0x80000178
  [      88]   0x00000000
  [      89]   0x00000000
  [      90]   0x8000007a
  [      91]   0x00000000
  [      92]   0x00000000
  [      93]   0x80000034
  [      94]   0x00000000
  [      95]   0x00000000
  [      96]   0x800000e0
  [      97]   0x80000056
  [      98]   0x00000000
  [      99]   0x800001c5
  [     100]   0x00000000
  [     101]   0x00000000
  [     102]   0x00000000
  [     103]   0x80000061
  [     104]   0x800001ad
  [     105]   0x00000000
  [     106]   0x00000000
  [     107]   0x80000148
  [     108]   0x800000b0
  [     109]   0x00000000
  [     110]   0x00000000
  [     111]   0x8000008a
  [     112]   0x00000000
  [     113]   0x00000000
  [     114]   0x00000000
  [     115]   0x00000000
  [     116]   0x00000000
  [     117]   0x800001c2
  [     118]   0x800000de
  [     119]   0x800001da
  [     120]   0x00000000
  [     121]   0x800001ac
  [     122]   0x80000109
  [     123]   0x00000000
  [     124]   0x00000000
  [     125]   0x00000000
  [     126]   0x00000000
  [     127]   0x80000092
  [     128]   0x80000163
  [     129]   0x80000127
  [     130]   0x00000000
  [     131]   0x80000057
  [     132]   0x00000000
  [     133]   0x8000010c
  [     134]   0x00000000
  [     135]   0x80000139
  [     136]   0x00000000
  [     137]   0x00000000
  [     138]   0x800000f4
  [     139]   0x00000000
  [     140]   0x800001a2
  [     141]   0x00000000
  [     142]   0x800001a5
  [     143]   0x00000000
  [     144]   0x800001b9
  [     145]   0x00000000
  [     146]   0x00000000
  [     147]   0x00000000
  [     148]   0x800000d8
  [     149]   0x00000000
  [     150]   0x80000000
  [     151]   0x00000000
  [     152]   0x00000000
  [     153]   0x00000000
  [     154]   0x00000000
  [     155]   0x800001b5
  [     156]   0x00000000
  [     157]   0x800000a1
  [     158]   0x00000000
  [     159]   0x00000000
  [     160]   0x8000012c
  [     161]   0x00000000
  [     162]   0x00000000
  [     163]   0x00000000
  [     164]   0x00000000
  [     165]   0x8000016e
  [     166]   0x00000000
  [     167]   0x00000000
  [     168]   0x00000000
  [     169]   0x00000000
  [     170]   0x80000190
  [     171]   0x00000000
  [     172]   0x8000013e
  [     173]   0x00000000
  [     174]   0x00000000
  [     175]   0x00000000
  [     176]   0x800000d7
  [     177]   0x800000eb
  [     178]   0x00000000
  [     179]   0x00000000
  [     180]   0x00000000
  [     181]   0x00000000
  [     182]   0x800001df
  [     183]   0x00000000
  [     184]   0x80000194
  [     185]   0x00000000
  [     186]   0x00000000
  [     187]   0x00000000
  [     188]   0x80000153
  [     189]   0x80000195
  [     190]   0x80000177
  [     191]   0x80000002
  [     192]   0x00000000
  [     193]   0x80000016
  [     194]   0x00000000
  [     195]   0x00000000
  [     196]   0x8000005b
  [     197]   0x00000000
  [     198]   0x8000003e
  [     199]   0x80000158
  [     200]   0x800001e7
  [     201]   0x80000198
  [     202]   0x00000000
  [     203]   0x80000105
  [     204]   0x80000036
  [     205]   0x800001de
  [     206]   0x00000000
  [     207]   0x00000000
  [     208]   0x800001a7
  [     209]   0x00000000
  [     210]   0x00000000
  [     211]   0x00000000
  [     212]   0x80000181
  [     213]   0x00000000
  [     214]   0x00000000
  [     215]   0x00000000
  [     216]   0x00000000
  [     217]   0x00000000
  [     218]   0x00000000
  [     219]   0x800001f2
  [     220]   0x00000000
  [     221]   0x80000052
  [     222]   0x80000183
  [     223]   0x80000108
  [     224]   0x00000000
  [     225]   0x00000000
  [     226]   0x00000000
  [     227]   0x800001d5
  [     228]   0x800000e2
  [     229]   0x8000005f
  [     230]   0x00000000
  [     231]   0x00000000
  [     232]   0x00000000
  [     233]   0x800001e8
  [     234]   0x00000000
  [     235]   0x800000d3
  [     236]   0x00000000
  [     237]   0x00000000
  [     238]   0x00000000
  [     239]   0x00000000
  [     240]   0x00000000
  [     241]   0x00000000
  [     242]   0x00000000
  [     243]   0x00000000
  [     244]   0x80000049
  [     245]   0x800000f5
  [     246]   0x800000ef
  [     247]   0x800001a4
  [     248]   0x800000f6
  [     249]   0x00000000
  [     250]   0x800001eb
  [     251]   0x00000000
  [     252]   0x00000000
  [     253]   0x00000000
  [     254]   0x80000159
  [     255]   0x00000000

Virtual Address Trace
  VA 0x0308b24d (decimal: 50901581) --> In page 48: 0x1f68b24d
  VA 0x042351e6 (decimal: 69423590) --> page 66 is invalid
  VA 0x02feb67b (decimal: 50247291) --> In page 47: 0x0a9eb67b
  VA 0x0b46977d (decimal: 189175677) --> page 180 is invalid
  VA 0x0dbcceb4 (decimal: 230477492) --> In page 219: 0x1f2cceb4

For each virtual address, write down the physical address it translates to
OR write down that it is an out-of-bounds address (e.g., segfault).
```

4. Use the program to try out some other problems. Can you find the limits of where the program doesn’t work anymore? For example, what happens if the address-space size is bigger than physical memory?

If the address-space is bigger than physical memory, the virtual addresses may map to unexistent physical addresses.
