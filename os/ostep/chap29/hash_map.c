#include "mythreads.h"
#include <stdlib.h>
#include <stdio.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

typedef struct __map {
    unsigned int **values;
    unsigned char length;
} map;

void init(map *a, unsigned char length) {
    a->length = length;
    a->values = (unsigned int**) malloc(sizeof(unsigned int*) * length);
}
