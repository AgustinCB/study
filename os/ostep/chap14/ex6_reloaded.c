#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/time.h>

#define TRIES 1000

/**
 * My previous assumption was completely wrong! Reallocating IS more expensive.
 *
 * However, preallocating with realloc makes a difference. A huge difference:
 *
 * $ ./ex6
 * Total time spent allocating 100 elements in vector: 2
 * Total time spent allocating 1000 elements in vector: 6
 * Total time spent allocating 10000 elements in vector: 49
 * Total time spent allocating 100 elements in linked list: 1
 * Total time spent allocating 1000 elements in linked list: 10
 * Total time spent allocating 10000 elements in linked list: 104
 *
 * (All in microseconds)
 *
 * However, it's worth noticing that the operations that I'm testing are different:
 *
 * For vectors, I'm testing appending. For linked list, I'm testing prepending.
 *
 * They both have complexity O(1) in their data structure.
 * However, they aren't the same.
 *
 * If I wanted to test append vs append prepend vs prepend, it wouldn't be a fair
 * thing because I'd be testing O(n) vs O(1) operations.
 *
 * Another thing worth noticing: It's easier to free a vector than a linked list.
 * For a vector, it's complexity O(1). For a linked list, it's complexity O(n).
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
    int capacity;
    void (*add)(vector_t* self, int new);
};

void add(vector_t* self, int new) {
    self->size ++;
    if (self->size > self->capacity) {
        self->capacity <<= 1;
        self->content = (int*) realloc(self->content, sizeof(int) * self->capacity);
    }
    self->content[self->size-1] = new;
}

llist_t* llist_add(llist_t* self, int new) {
    llist_t* newItem = (llist_t*) malloc(sizeof(llist_t));
    newItem->value = new;
    newItem->next = self;
    return newItem;
}

void vector_print(vector_t v) {
    printf("Printing vector\n");
    for (int i = 0; i < v.size; i++) {
        printf("Element %d: %d\n", i, v.content[i]);
    }
}

void llist_print(llist_t* self) {
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
    long long int total;

    total = 0;
    for (int j = 0; j < TRIES; j++) {
        vector_t v;
        v.size = 0;
        v.capacity = 1;
        v.content = (int*) malloc(sizeof(int));
        v.add = add;
        gettimeofday(&tpBefore, NULL);
        for (int i = 0; i < 100; i++) {
            v.add(&v, i);
        }
        gettimeofday(&tpAfter, NULL);

        free(v.content);
        total += (tpAfter.tv_usec - tpBefore.tv_usec);
    }
    printf("Total time spent allocating 100 elements in vector: %lld\n", total/TRIES);

    total = 0;
    for (int j = 0; j < TRIES; j++) {
        vector_t v;
        v.size = 0;
        v.capacity = 1;
        v.content = (int*) malloc(sizeof(int));
        v.add = add;
        gettimeofday(&tpBefore, NULL);
        for (int i = 0; i < 1000; i++) {
            v.add(&v, i);
        }
        gettimeofday(&tpAfter, NULL);

        free(v.content);
        total += (tpAfter.tv_usec - tpBefore.tv_usec);
    }
    printf("Total time spent allocating 1000 elements in vector: %lld\n", total/TRIES);

    total = 0;
    for (int j = 0; j < TRIES; j++) {
        vector_t v;
        v.size = 0;
        v.capacity = 1;
        v.content = (int*) malloc(sizeof(int));
        v.add = add;
        gettimeofday(&tpBefore, NULL);
        for (int i = 0; i < 10000; i++) {
            v.add(&v, i);
        }
        gettimeofday(&tpAfter, NULL);

        free(v.content);
        total += (tpAfter.tv_usec - tpBefore.tv_usec);
    }
    printf("Total time spent allocating 10000 elements in vector: %lld\n", total/TRIES);

    total = 0;
    for (int j = 0; j < TRIES; j++) {
        llist_t* h = NULL;
        gettimeofday(&tpBefore, NULL);
        for (int i = 0; i < 100; i++) {
            h = llist_add(h, i);
        }
        gettimeofday(&tpAfter, NULL);

        llist_t* curr = h;
        llist_t* tmp;
        while (curr != NULL) {
            tmp = curr->next;
            free(curr);
            curr = tmp;
        }
        total += (tpAfter.tv_usec - tpBefore.tv_usec);
    }
    printf("Total time spent allocating 100 elements in linked list: %lld\n", total/TRIES);

    total = 0;
    for (int j = 0; j < TRIES; j++) {
        llist_t* h = NULL;
        gettimeofday(&tpBefore, NULL);
        for (int i = 0; i < 1000; i++) {
            h = llist_add(h, i);
        }
        gettimeofday(&tpAfter, NULL);

        llist_t* curr = h;
        llist_t* tmp;
        while (curr != NULL) {
            tmp = curr->next;
            free(curr);
            curr = tmp;
        }
        total += (tpAfter.tv_usec - tpBefore.tv_usec);
    }
    printf("Total time spent allocating 1000 elements in linked list: %lld\n", total/TRIES);

    total = 0;
    for (int j = 0; j < TRIES; j++) {
        llist_t* h = NULL;
        gettimeofday(&tpBefore, NULL);
        for (int i = 0; i < 10000; i++) {
            h = llist_add(h, i);
        }
        gettimeofday(&tpAfter, NULL);

        llist_t* curr = h;
        llist_t* tmp;
        while (curr != NULL) {
            tmp = curr->next;
            free(curr);
            curr = tmp;
        }
        total += (tpAfter.tv_usec - tpBefore.tv_usec);
    }
    printf("Total time spent allocating 10000 elements in linked list: %lld\n", total/TRIES);
}
