#include <stdio.h>

#include "boX11.h"

int main(int argc, char **argv)
{
    char buff[128];

    char *arg = argc > 1 ? argv[1] : ".*";
    HWND *ret = getWinsByNameEx(arg);
    while (*ret) {
        GetWindowText(*ret, buff, sizeof(buff)-1);
        printf("%d -> %s\n", *ret, buff);
        ret++;
    }
    return 0;
}
