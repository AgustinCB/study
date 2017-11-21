#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/wait.h>

/*
 * Wait returns the pid of the child in the parent.
 * When called in the hcild, it returns -1 at the moment.
 * That means that there was an error in the call.
 * If I were to check errno, I'd see that the code number is ECHILD.
 * Which means that the process didn't have a child to wait for.
 */

int main(int argc, char *argv[]) {
    printf("hello world (pid: %d)\n", (int) getpid());
    int rc = fork();
    if (rc < 0) {
        fprintf(stderr, "for failed\n");
        exit(1);
    } else if (rc == 0) {
        int wc = wait(NULL);
        printf("CHILD (pid: %d) %d\n", (int) getpid(), wc);
    } else {
        int wc = wait(NULL);
        printf("PARENT: %d\n", wc);
    }
    return 0;
}
