#define _GNU_SOURCE
#include <sched.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/time.h>

#define TRIES 10000
#define CPU 1

int main(int argc, char *argv[]) {
    long int total = 0;
    int fd1[2];
    int fd2[2];
    cpu_set_t set;

    pipe(fd1);
    pipe(fd2);
    
    CPU_ZERO(&set);

    switch(fork()) {
        case -1:
            fprintf(stderr, "ERROR FORKING");
            exit(1);
            break;
        case 0: // Child
            CPU_SET(1, &set);
            if (sched_setaffinity(getpid(), sizeof(set), &set) == -1) {
                fprintf(stderr, "ERROR SETTING AFFINITY IN CHILD");
                exit(1);
            }
            close(fd1[0]);
            close(fd2[1]);
            for (int i=0; i<TRIES; i++) {
                char throw[2];
                write(fd1[1], "1", 2);
                read(fd2[0], throw, 2);
            }
            close(fd1[1]);
            close(fd2[0]);
            exit(0);
        default: // Parent
            CPU_SET(1, &set);
            if (sched_setaffinity(getpid(), sizeof(set), &set) == -1) {
                fprintf(stderr, "ERROR SETTING AFFINITY IN CHILD");
                exit(1);
            }
            close(fd1[1]);
            close(fd2[0]);
            for (int i=0; i<TRIES; i++) {
                struct timeval tpBefore;
                struct timeval tpAfter;
                char throw[2];
                gettimeofday(&tpBefore, NULL);
                read(fd1[0], throw, 2);
                write(fd2[1], "1", 2);
                gettimeofday(&tpAfter, NULL);
                total += tpAfter.tv_usec - tpBefore.tv_usec;
            }
            close(fd1[0]);
            close(fd2[1]);
            break;
    }

    printf("The average context switch cost in microseconds is: %f\n", ((double)total) / TRIES);
}
