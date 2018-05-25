#include <sys/time.h>
#include <stdlib.h>

typedef struct __counter_t {
    int value;
} counter_t;

void init(counter_t *c) {
    c->value = 0;
}

void increment(counter_t *c) {
    c->value++;
}

void decrement(counter_t *c) {
    c->value--;
}

int get(counter_t *c) {
    return c->value;
}

int main(int argc, char *argv[]) {
    counter_t counter;
    struct timeval tpBefore;
    struct timeval tpAfter;
    int threads = atoi(argv[1]);
    init(&counter);
    gettimeofday(&tpBefore, NULL);
    for (int t = 0; t < threads; t++) {
        switch(fork()) {
        case -1:
            fprintf(stderr, "ERROR FORKING");
            exit(1);
            break;
        case 0: // Child
            return count(counter, 100);
        default: // Parent
        }

    }
    while(wait(NULL)>0) ;
    gettimeofday(&tpAfter, NULL);
    long int total = tpAfter.tv_usec - tpBefore.tv_usec;
    printf("Time taken: %d\n", total);
}
