#include "mythreads.h"
#include <stdlib.h>
#include <stdio.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

typedef struct __counter_t {
    int value;
    pthread_mutex_t lock;
} counter_t;

void init(counter_t *c) {
    c->value = 0;
    Pthread_mutex_init(&c->lock, NULL);
}

void increment(counter_t *c) {
    Pthread_mutex_lock(&c->lock);
    c->value++;
    Pthread_mutex_unlock(&c->lock);
}

void decrement(counter_t *c) {
    Pthread_mutex_lock(&c->lock);
    c->value--;
    Pthread_mutex_unlock(&c->lock);
}

int get(counter_t *c) {
    Pthread_mutex_lock(&c->lock);
    int v = c->value;
    Pthread_mutex_unlock(&c->lock);
    return v;
}

void count(counter_t *c, int limit) {
    if (get(c) >= limit) return;
    increment(c);
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
            count(&counter, 100);
            return 0;
        default: // Parent
            ;
        }

    }
    while(wait(NULL)>0) ;
    gettimeofday(&tpAfter, NULL);
    long int total = tpAfter.tv_usec - tpBefore.tv_usec;
    printf("Count: %d\n", get(&counter));
    printf("Time taken: %d microseconds\n", total);
}
