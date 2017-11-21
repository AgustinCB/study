#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>

/*
 * The variable x gets copied into the child thread.
 * Modifying its value in the thread doesn't seem to bother the parent's variable at all.
 * They both have different values at all.
 */

int main(int argc, char *argv[]) {
    printf("hello world (pid: %d)\n", (int) getpid());
    int x = 100;
    int rc = fork();
    if (rc < 0) {
        fprintf(stderr, "for failed\n");
        exit(1);
    } else if (rc == 0) {
        printf("(child) hello, I'm child (pid: %d) and x's value is %d\n", (int) getpid(), x);
        x = 90;
        printf("(child) x value is now %d\n", x);
    } else {
        printf("(parent) x value is %d\n", x);
        x = 80;
        printf("(parent) x value is now %d\n", x);
        printf("(parent) hello, I'm parent of %d (wc: %d) (pid: %d)\n", rc, (int) getpid());
    }
    return 0;
}
