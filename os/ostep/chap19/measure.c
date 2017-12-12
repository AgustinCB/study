#define _GNU_SOURCE
#include <sched.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/time.h>
#include <unistd.h>

int main(int argc, char* argv[]) {
    if (argc != 3) {
        printf("Usage: measure [pages] [tries]\n");
        exit(1);
    }
    long int PAGESIZE = sysconf(_SC_PAGESIZE);
    long int jump = PAGESIZE/sizeof(int);
    int pages = atoi(argv[1]);
    int tries = atoi(argv[2]);
    struct timeval tpBefore;
    struct timeval tpAfter;
    double total = 0.0;
    int min = 10;
    int* a = (int*) malloc(sizeof(int) * pages * jump);
    cpu_set_t set;
    CPU_ZERO(&set);
    CPU_SET(1, &set);
    if (sched_setaffinity(getpid(), sizeof(set), &set) == -1) {
        fprintf(stderr, "ERROR SETTING AFFINITY IN CHILD");
        exit(1);
    }

    for (int i = 0; i < pages * jump; i++) {
        a[i] = 0;
    }
    double res = -1;

    while (res < 0) {
        total = 0.0;
        for (int i = 0; i<tries; i++) {
            gettimeofday(&tpBefore, NULL);
            for (int k = 0; k < min; k ++) {
                for (int j = 0; j < pages * jump; j+=jump) {
                    a[j] = 1;
                }
            }
            gettimeofday(&tpAfter, NULL);
            total += ((double)tpAfter.tv_usec - tpBefore.tv_usec) / (pages * min);
        }
        res = total/tries;
    }

    printf("%d\t%d\t%f\n", pages, tries, res);
    free(a);
}
