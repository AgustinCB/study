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
    struct __l_node *next;
} l_node;

void init(l_node *node, int value) {
    node->value = value;
    Pthread_mutex_init(&node->lock, NULL);
    node->next = NULL;
}

l_node* find_last(l_node* from) {
    l_node *curr = from;
    l_node *next = NULL;
    Pthread_mutex_lock(&curr->lock);
    while (curr->next != NULL) {
        next = curr->next;
        Pthread_mutex_unlock(&curr->lock);
        curr = next;
        Pthread_mutex_lock(&curr->lock);
    }
    Pthread_mutex_unlock(&curr->lock);
    return curr;
}

void add(l_node *prev, l_node *new) {
    Pthread_mutex_lock(&prev->lock);
    if (prev->next != NULL) {
        if (new->next == NULL) new->next = prev->next;
        else find_last(new)->next = prev->next;
    }
    prev->next = new;
    Pthread_mutex_unlock(&prev->lock);
}

l_node* find(int value, l_node* head) {
    l_node *curr = head;
    l_node *next = NULL;
    Pthread_mutex_lock(&curr->lock);
    while (curr != NULL && curr->value != value) {
        next = curr->next;
        Pthread_mutex_unlock(&curr->lock);
        curr = next;
        Pthread_mutex_lock(&curr->lock);
    }
    Pthread_mutex_unlock(&curr->lock);
    return curr;
}

int main(int argc, char *argv[]) {
    l_node head;
    struct timeval tpBefore;
    struct timeval tpAfter;
    int threads = atoi(argv[1]);
    init(&head, -1);
    gettimeofday(&tpBefore, NULL);
    for (int t = 0; t < threads; t++) {
        l_node next;
        switch(fork()) {
        case -1:
            fprintf(stderr, "ERROR FORKING");
            exit(1);
            break;
        case 0: // Child
            init(&next, 0);
            add(&head, &next);
            return 0;
        default: // Parent
            ;
        }

    }
    while(wait(NULL)>0) ;
    gettimeofday(&tpAfter, NULL);
    long int total = tpAfter.tv_usec - tpBefore.tv_usec;
    printf("Time taken: %d microseconds\n", total);
}
