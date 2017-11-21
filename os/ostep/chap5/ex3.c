#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/wait.h>

/*
 * Using sleep is the only way I can think of to ensure that the child is always the first one.
 * It forces the thread to change it's status to "interruptible sleep," which works similar to the IO blocking:
 * It hands over the CPU and waits still the number the of second received as parameter passes.
 * Then it's sets the status of the process to READY again.
 */

int main(int argc, char *argv[]) {
    printf("hello world (pid: %d)\n", (int) getpid());
    int rc = fork();
    if (rc < 0) {
        fprintf(stderr, "for failed\n");
        exit(1);
    } else if (rc == 0) {
        printf("(child) hola\n");
    } else {
        sleep(1);
        printf("(parent) hola\n");
    }
    return 0;
}
