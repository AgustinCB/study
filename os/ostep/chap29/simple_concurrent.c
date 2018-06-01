#include "mythreads.h"
#include <stdlib.h>
#include <stdio.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

typedef struct __l_node {
    int value;
    pthread_mutex_t lock;
    __l_node *next;
} l_node;

void init(l_node *node, int value) {
    node->value = value;
    Pthread_mutex_init(&node->lock, NULL);
    node->next = NULL;
}

l_node* find_last(l_node* from) {
    l_node *curr = from;
    while (curr->next != NULL) curr = curr->next;
    return curr;
}

void add(l_node *prev, l_node *new) {
    if (prev->next != NULL) {
        if (new->next == NULL) new->next = prev->next;
        else find_last(new)->next = prev->next;
    }
    prev->next = new;
}

l_node* find(int value, l_node* head) {
    l_node *curr = head;
    while (curr != NULL && curr->value != value) curr = curr->next;
    return curr;
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
