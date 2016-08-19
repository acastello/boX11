#include "boX11.h"
#include <stdio.h>
#include <unistd.h>

int main(void)
{
    char buff[1024];
    ssize_t s;
    while ((s = read(STDIN_FILENO, buff, sizeof(buff)-1)) > 0) {
        buff[s] = '\0';
        getWins(buff, BOX_BYNAME | BOX_REGEX);
    }
    return 0;
}
