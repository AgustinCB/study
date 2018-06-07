#include "mythreads.h"
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

typedef struct __key_value_pair {
    int value;
    char* key;
} key_value_pair;

typedef struct __key_value_pair_array {
    key_value_pair *content;
    unsigned int length;
    unsigned int capacity;
} key_value_pair_array;

typedef struct __map {
    key_value_pair_array *values;
    unsigned char length;
} map;

typedef struct __maybe_int {
    bool defined;
    int value;
} maybe_int;

void init_maybe_int(maybe_int *r, bool defined, int value) {
    r->defined = defined;
    r->value = value;
}

void init_kv_array(key_value_pair_array *a) {
    a->length = 0;
    a->capacity = 1;
    a->content = (key_value_pair*) malloc(sizeof(key_value_pair) * a->capacity);
}

void init(map *a, unsigned char length) {
    a->length = length;
    a->values = (key_value_pair_array*) malloc(sizeof(key_value_pair_array) * length);
}

maybe_int get(map a, char* key) {
    key_value_pair_array possibilities = a.values[hash(key)];
    maybe_int result;
    if (possibilities.length == 0) {
        init_maybe_int(&result, false, 0);
        return result;
    }
    for (int i = 0; i < possibilities.capacity; i++) {
        if (strcmp(possibilities.content[i].key, key) == 0) {
            init_maybe_int(&result, true, possibilities.content[i].value);
            return result;
        }
    }

    init_maybe_int(&result, false, 0);
    return result;
}
