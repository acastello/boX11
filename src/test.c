#include "boX11.h"
#include <stdio.h>

int main(int argc, char **argv)
{
    char *filter = argc > 1 ? argv[1] : ".*";
    HWND *ret = getWins(filter, BOX_BYNAME | BOX_REGEX);

    while (getchar())
        printf("%x -> %d\n", ret, *ret);

    return 0;
}
