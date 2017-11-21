#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/wait.h>
#include <sys/types.h>

int main(int argc, char *argv[]) {
    printf("hello world (pid: %d)\n", (int) getpid());
    int fd[2];
    char *string = "hola";
    pipe(fd);
    pid_t rc = fork();
    if (rc < 0) {
        fprintf(stderr, "for failed\n");
        exit(1);
    } else if (rc == 0) {
        /* Child process closes up input side of pipe */
        close(fd[0]);
        write(fd[1], string, (strlen(string)+1));
        close(fd[1]);
    } else {
        /* Parent process closes up output side of pipe */
        close(fd[1]);
        char readbuffer[80];
        int nbytes = read(fd[0], readbuffer, sizeof(readbuffer));
        close(fd[0]);
        printf("received string %s %d\n", readbuffer, nbytes);
    }
    return 0;
}
