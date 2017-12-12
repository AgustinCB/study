#include <stdio.h>
#include <stdlib.h>

/**
 * In this case, the program does fail on execution:
 * 
 * *** Error in `./ex5': free(): invalid pointer: 0x000056490c3f9328 ***
 * Aborted (core dumped)
 *
 * Valgrind even gives me the culprit:
 *
 * ==19884== Invalid free() / delete / delete[] / realloc()
 * ==19884==    at 0x4C2E14B: free (in /usr/lib/valgrind/vgpreload_memcheck-amd64-linux.so)
 * ==19884==    by 0x108708: main (ex5.c:11)
 * ==19884==  Address 0x51f1108 is 200 bytes inside a block of size 400 alloc'd
 * ==19884==    at 0x4C2CE5F: malloc (in /usr/lib/valgrind/vgpreload_memcheck-amd64-linux.so)
 * ==19884==    by 0x1086F2: main (ex5.c:10)
 */

int main(int argc, char* argv[]) {
    int *array = (int*) malloc(sizeof(int) * 100);
    free(&(array[50]));
    printf("Value: %d\n", array[0]);
}
