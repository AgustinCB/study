#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/wait.h>

/*
 * It doesn't print anything at all, nor fails in any memorable way.
 */

int main(int argc, char *argv[]) {
    printf("hello world (pid: %d)\n", (int) getpid());
    int rc = fork();
    if (rc < 0) {
        fprintf(stderr, "for failed\n");
        exit(1);
    } else if (rc == 0) {
        close(STDOUT_FILENO);
        printf("TRY");
    } else {
        int wc = waitpid(-1, NULL, 0);
        printf("PARENT: %d\n", wc);
    }
    return 0;
}
