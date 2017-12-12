#include <stddef.h>
#include <stdio.h>

/**
 * Trying to dereferencing a pointer set to NULL gives a segmentation fault.
 *
 * Running it using gdb shows the following:
 *
 * (gdb) run
 * Starting program: /home/agustin/study/os/ostep/chap14/ex1
 *
 * Program received signal SIGSEGV, Segmentation fault.
 * 0x0000555555554665 in main (argc=1, argv=0x7fffffffe178) at ex1.c:10
 * 10	    printf("HOLA %d\n", *pointer);
 *
 * If I run it using valgrind it reports that there's a an error in the follwoing way:
 *
 * ==19415== Invalid read of size 4
 * ==19415==    at 0x108665: main (ex1.c:28)
 * ==19415==  Address 0x0 is not stack'd, malloc'd or (recently) free'd
 *
 * NULL in C is just an alias for zero. That message is saying that that memory address was
 * never malloc'd. I.e. that we're trying to dereferencing an address never reserved.
 */

int main(int argc, char* argv[]) {
    int* pointer = NULL;
    printf("HOLA %d\n", *pointer);
}
