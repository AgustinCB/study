#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/wait.h>

/*
 * wait is particular case of waitpid.
 * waitpid seems to be interesting for when you need to wait for an specific child to finish.
 * or when you want an slightly different behavior of wait
 * for example, return immediately if no child finished.
 */

int main(int argc, char *argv[]) {
    printf("hello world (pid: %d)\n", (int) getpid());
    int rc = fork();
    if (rc < 0) {
        fprintf(stderr, "for failed\n");
        exit(1);
    } else if (rc == 0) {
        int wc = waitpid(-1, NULL, 0);
        printf("CHILD (pid: %d) %d\n", (int) getpid(), wc);
    } else {
        int wc = waitpid(-1, NULL, 0);
        printf("PARENT: %d\n", wc);
    }
    return 0;
}
