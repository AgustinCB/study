#include <stdlib.h>

/**
 * As before, the program finishes with no problem. Scary.
 * Even using gdb doesn't find anything.
 *
 * No, the program is not correct. In arrays with a size of N, the maximum index is N-1.
 * And the memory is never freed.
 *
 * Valgrind, on the other hand, does a great job:
 *
 * ==19677== Invalid write of size 4
 * ==19677==    at 0x108671: main (ex3.c:30)
 * ==19677==  Address 0x51f11d0 is 0 bytes after a block of size 400 alloc'd
 * ==19677==    at 0x4C2CE5F: malloc (in /usr/lib/valgrind/vgpreload_memcheck-amd64-linux.so)
 * ==19677==    by 0x108662: main (ex3.c:29)
 * ==19677==
 * ==19677==
 * ==19677== HEAP SUMMARY:
 * ==19677==     in use at exit: 400 bytes in 1 blocks
 * ==19677==   total heap usage: 1 allocs, 0 frees, 400 bytes allocated
 * ==19677==
 * ==19677== 400 bytes in 1 blocks are definitely lost in loss record 1 of 1
 * ==19677==    at 0x4C2CE5F: malloc (in /usr/lib/valgrind/vgpreload_memcheck-amd64-linux.so)
 * ==19677==    by 0x108662: main (ex3.c:29)
 */

int main(int argc, char* argv[]) {
    int *array = (int*) malloc(sizeof(int) * 100);
    array[100] = 0;
}
