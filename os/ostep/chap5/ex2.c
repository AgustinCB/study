#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/wait.h>

/*
 * It seems to open a file descriptor for the child process with a random number.
 * Writing works as expected, although if you dont "wait" in the parent, the order in which it happens is undeterministic.
 */

int main(int argc, char *argv[]) {
    printf("hello world (pid: %d)\n", (int) getpid());
    int f = open("./test", O_CREAT | O_WRONLY | O_TRUNC, S_IRWXU);
    int rc = fork();
    if (rc < 0) {
        fprintf(stderr, "for failed\n");
        exit(1);
    } else if (rc == 0) {
        printf("(child) hello, I'm child (pid: %d) and f's value is %d\n", (int) getpid(), f);
        printf("(child) WRITING %d BYTES\n", write(f, "child", 5));
    } else {
        printf("(parent) hello, I'm parent of %d (wc: %d) (pid: %d) and f's value is %d\n", rc, (int) getpid(), f);
        printf("(parent) WRITING %d BYTES\n", write(f, "parent", 6));
    }
    close(f);
    return 0;
}
