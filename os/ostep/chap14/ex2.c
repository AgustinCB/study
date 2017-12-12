#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

/**
 * The program finishes with no problem.
 * The error code is zero and there's no sign that there was something wrong.
 *
 * Even running it with gdb:
 *
 * (gdb) run
 * Starting program: /home/agustin/study/os/ostep/chap14/ex2
 * HOLA 42
 * [Inferior 1 (process 19544) exited normally]
 *
 * valgrind does find a problem, though:
 *
 * ==19560== HEAP SUMMARY:
 * ==19560==     in use at exit: 4 bytes in 1 blocks
 * ==19560==   total heap usage: 2 allocs, 1 frees, 1,028 bytes allocated
 * ==19560==
 * ==19560== 4 bytes in 1 blocks are definitely lost in loss record 1 of 1
 * ==19560==    at 0x4C2CE5F: malloc (in /usr/lib/valgrind/vgpreload_memcheck-amd64-linux.so)
 * ==19560==    by 0x1086B2: main (ex2.c:9)
 */

int main(int argc, char* argv[]) {
    int* pointer = (int*) malloc(sizeof(int));
    *pointer = 42;
    printf("HOLA %d\n", *pointer);
}
