#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/time.h>

#define TRIES 10000

int main(int argc, char *argv[]) {
    long int total = 0;

    for (int i=0; i<TRIES; i++) {
        struct timeval tpBefore;
        struct timeval tpAfter;
        gettimeofday(&tpBefore, NULL);
        read(0, NULL, 0);
        gettimeofday(&tpAfter, NULL);
        total += tpAfter.tv_usec - tpBefore.tv_usec;
    }
    printf("The average system call cost in microseconds is: %f\n", ((double)total) / TRIES);
}
