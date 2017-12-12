#include <stdio.h>
#include <stdlib.h>

/**
 * As before, the program finishes with no problem.
 *
 * Valgrind, as usual, is much more reliable:
 *
 * ==19755== Invalid read of size 4
 * ==19755==    at 0x108707: main (ex4.c:11)
 * ==19755==  Address 0x51f1040 is 0 bytes inside a block of size 400 free'd
 * ==19755==    at 0x4C2E14B: free (in /usr/lib/valgrind/vgpreload_memcheck-amd64-linux.so)
 * ==19755==    by 0x108702: main (ex4.c:10)
 * ==19755==  Block was alloc'd at
 * ==19755==    at 0x4C2CE5F: malloc (in /usr/lib/valgrind/vgpreload_memcheck-amd64-linux.so)
 * ==19755==    by 0x1086F2: main (ex4.c:9)
 */

int main(int argc, char* argv[]) {
    int *array = (int*) malloc(sizeof(int) * 100);
    free(array);
    printf("Value: %d\n", array[0]);
}
