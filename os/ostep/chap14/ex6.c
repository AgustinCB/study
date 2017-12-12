#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/time.h>

/**
 * In general, a vector seems to be more responsive than a linked list.
 * And that keeping in mind that my implementation is very naive:
 * I could, for example, introduce the concept of capacity. Preallocate some amount of memory
 * and, on adding, check if I have enough capacity to add. If not, then allocate more.
 * That would likely increase more the performance (although at the expenses of memory).
 *
 * It looks like, however, realloc is more performant than malloc in this kind of scenarios.
 */

typedef struct llist_t llist_t;
struct llist_t {
    int value;
    llist_t* next;
};

typedef struct vector_t vector_t;
struct vector_t {
    int* content;
    int size;
    void (*add)(vector_t* self, int new);
};

void add(vector_t* self, int new) {
    self->size ++;
    self->content = (int*) realloc(self->content, sizeof(int) * self->size);
    self->content[self->size-1] = new;
}

llist_t* addToLlist(llist_t* self, int new) {
    llist_t* newItem = (llist_t*) malloc(sizeof(llist_t));
    newItem->value = new;
    newItem->next = self;
    return newItem;
}

void printVector(vector_t v) {
    printf("Printing vector\n");
    for (int i = 0; i < v.size; i++) {
        printf("Element %d: %d\n", i, v.content[i]);
    }
}

void printLlist(llist_t* self) {
    printf("Printing linked list\n");
    llist_t* curr = self;
    int i = 0;
    while (curr != NULL) {
        printf("Element %d: %d\n", i, curr->value);
        i++;
        curr = curr->next;
    }
}

int main(int argc, char* argv[]) {
    struct timeval tpBefore;
    struct timeval tpAfter;
    vector_t v;
    v.size = 0;
    v.content = (int*) malloc(0);
    v.add = add;

    gettimeofday(&tpBefore, NULL);
    for (int i = 0; i < 1000; i++) {
        v.add(&v, i);
    }
    gettimeofday(&tpAfter, NULL);
    printf("Total time spent allocating 1000 elements in vector: %f\n", ((double)(tpAfter.tv_usec - tpBefore.tv_usec)));

    free(v.content);

    llist_t* h = NULL;

    gettimeofday(&tpBefore, NULL);
    for (int i = 0; i < 1000; i++) {
        h = addToLlist(h, i);
    }
    gettimeofday(&tpAfter, NULL);
    printf("Total time spent allocating 1000 elements in linked list: %f\n", ((double)(tpAfter.tv_usec - tpBefore.tv_usec)));

    llist_t* curr = h;
    llist_t* tmp;
    while (curr != NULL) {
        tmp = curr->next;
        free(curr);
        curr = tmp;
    }
}
