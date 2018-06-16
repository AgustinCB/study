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
    size_t length;
} map;

typedef struct __maybe_int {
    bool defined;
    int value;
} maybe_int;

unsigned int hash(const char *key, size_t length) {
    int i;
    unsigned int result = 0;
    for (i = 0; key[i] != '\0'; i++) {
        result += key[i++];
        result += result << 10;
        result ^= result >> 6;
    }
    result += key[i++];
    result += result << 10;
    result ^= result >> 6;
    return result % length;
}

void init_maybe_int(maybe_int *r, const bool defined, const int value) {
    r->defined = defined;
    r->value = value;
}

void init_kv_array(key_value_pair_array *a) {
    a->length = 0;
    a->capacity = 1;
    a->content = (key_value_pair*) malloc(sizeof(key_value_pair) * a->capacity);
}

void init(map *a, const size_t length) {
    a->length = length;
    a->values = (key_value_pair_array*) malloc(sizeof(key_value_pair_array) * length);
    for (int i=0; i < length; i++) init_kv_array(&(a->values[i]));
}

void increase_kv_array_capacity(key_value_pair_array *a) {
    a->capacity *= 2;
    key_value_pair *new_content = (key_value_pair*) realloc(a->content, sizeof(key_value_pair) * a->capacity);
    a->content = new_content;
}

maybe_int get(map a, const char* key) {
    const key_value_pair_array possibilities = a.values[hash(key, a.length)];
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

void set(map a, const char* key, const int value) {
    key_value_pair_array *possibilities = &(a.values[hash(key, a.length)]);
    int i=0;
    while (i < possibilities->length && strcmp(possibilities->content[i].key, key) != 0) i++; 
    if (i >= possibilities->length) {
        if (possibilities->length >= possibilities->capacity) {
            increase_kv_array_capacity(possibilities);
        }
        possibilities->length++;
    }
    key_value_pair *new = &(possibilities->content[i]);
    new->value = value;
    const int length = strlen(key);
    new->key = (char*) malloc(sizeof(char) * length);
    strncpy(new->key, key, length);
}

void del(map a, const char* key) {
    key_value_pair_array possibilities = a.values[hash(key, a.length)];
    if (possibilities.length == 0) return;
    int i;
    for (i = 0; i < possibilities.length; i++) {
        if (strcmp(possibilities.content[i].key, key) == 0) {
            free(possibilities.content[i].key);
            free(&possibilities.content[i]);
            break;
        }
    }
    for (int j = i + 1; j < possibilities.length; j++) {
        possibilities.content[j-1] = possibilities.content[j];
    }
}

int main () {
    map a;
    init(&a, 1024);
    printf("Setting\n");
    set(a, "uno", 1);
    set(a, "two", 1);
    set(a, "two", 2);
    printf("Getting\n");
    printf("For uno: %d\n", get(a, "uno").value);
    printf("For two: %d\n", get(a, "two").value);
    printf("Deleting\n");
    del(a, "uno");
    del(a, "two");
    printf("Is uno set: %d\n", (int) get(a, "uno").defined);
    printf("Is two set: %d\n", (int) get(a, "two").defined);
}
