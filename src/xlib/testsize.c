#include <X11/Xlib.h>
#include <stdio.h>

int main(void)
{
    printf("sizeof modMask: %d\n", sizeof(ShiftMask));
    printf("sizeof KeySym: %d\n", sizeof(KeySym));
    return 0;
}
