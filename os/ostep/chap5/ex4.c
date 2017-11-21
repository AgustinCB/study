#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/wait.h>

/*
 * I think that each variant of exec is a different interface for the same case.
 * Which to use depends on which interface makes more sense in your program.
 */

int main(int argc, char *argv[]) {
    printf("hello world (pid: %d)\n", (int) getpid());
    int rc = fork();
    if (rc < 0) {
        fprintf(stderr, "for failed\n");
        exit(1);
    } else if (rc == 0) {
        char *args[2] = {"ls", NULL};
        execv("/bin/ls", args);
    } else {
        wait(NULL);
    }
    return 0;
}
