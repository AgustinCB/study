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
    struct timeval tpBefore;
    struct timeval tpAfter;
    int threads = atoi(argv[1])
    gettimeofday(&tpBefore, NULL);
    for (int t = 0; t < threads; t++) {
        switch(fork()) {
        case -1:
            fprintf(stderr, "ERROR FORKING");
            exit(1);
            break;
        case 0: // Child
        default: // Parent
        }

    }
    gettimeofday(&tpAfter, NULL);
}
