#include <stdlib.h>
#include <stdio.h>
#include <string.h>

typedef struct t_node {
    char *value;
    int length;
    struct t_node *next;
} node;

int min(int f, int s) {
    return f > s ? s : f;
}

void init_node(node *n, const char* value, const node* next) {
    if (value != NULL) {
        n->length = strlen(value);
    } else {
        n->length = 0;
    }
    n->value = (char*) malloc(sizeof(char) * (n->length+1));
    strncat(n->value, value, n->length);
    n->next = next;
}

void free_node(node *n) {
    free(n->value);
    free(n);
}

node* find_last(const node *head) {
    if (head == NULL) return NULL;
    node *tmp = head;
    while (tmp->next != NULL) tmp = tmp->next;
    return tmp;
}

node* find(const node *head, const char *value) {
    if (head == NULL) return NULL;
    node *tmp = head;
    int l = strlen(value);
    while (tmp != NULL && !(tmp->length == l && strncmp(tmp->value, value, l) == 0)) {
        tmp = tmp->next;
    }
    return tmp;
}

node* find_prev(const node *head, const char *value) {
    if (head == NULL) return NULL;
    node *tmp = head;
    node *prev = NULL;
    int l = strlen(value);
    while (!(tmp != NULL && tmp->length == l && strncmp(tmp->value, value, l) == 0)) {
        printf("ONE LOOP %d %s\n", tmp->length, tmp->value);
        prev = tmp;
        tmp = tmp->next;
    }
    if (tmp == NULL) return NULL;
    return prev;
}

void add_to_list(node *head, const char *value) {
    node *tail = find_last(head);
    if (tail != NULL) {
        node *new_node = malloc(sizeof(node));
        init_node(new_node, value, NULL);
        tail->next = new_node;
    }
}

node* delete(node *head, const char *value) {
    node *prev = find_prev(head, value);
    node *victim = find(head, value);
    if (victim == NULL) return head;
    if (prev == NULL) {
        node *new_head = victim->next;
        free_node(victim);
        return new_head;
    } else {
        prev->next = victim->next;
        free_node(victim);
        return head;
    }
}

int main() {
    node *head = (node*) malloc(sizeof(node));
    printf("INITING\n");
    init_node(head, "uno", NULL);
    printf("ADDING\n");
    add_to_list(head, "two");
    add_to_list(head, "three");

    printf("TRY TO FIND FIRST %s\n", find(head, "uno")->value);
    printf("TRY TO FIND SECOND %s\n", find(head, "two")->value);
    printf("TRY TO FIND THIRD %s\n", find(head, "three")->value);

    head = delete(head, "uno");
    printf("NEW HEAD %s\n", head->value);
    printf("TRY TO FIND FIRST %d\n", find(head, "uno"));
    printf("TRY TO FIND SECOND %s\n", find(head, "two")->value);
    printf("TRY TO FIND THIRD %s\n", find(head, "three")->value);

    head = delete(head, "two");
    printf("NEW HEAD %s\n", head->value);
    printf("TRY TO FIND FIRST %d\n", find(head, "uno"));
    printf("TRY TO FIND SECOND %d\n", find(head, "two"));
    printf("TRY TO FIND THIRD %s\n", find(head, "three")->value);

    head = delete(head, "three");
    printf("NEW HEAD %d\n", head);
    printf("TRY TO FIND FIRST %d\n", find(head, "uno"));
    printf("TRY TO FIND SECOND %d\n", find(head, "two"));
    printf("TRY TO FIND THIRD %d\n", find(head, "three"));
}
