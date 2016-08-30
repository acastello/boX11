#include <stdio.h>
#include <unistd.h>

#include <X11/Xlib.h>

#define BIT(arr, i) (arr[i/8]>>(i%8)&0x1)

int main(void)
{
    Display *dpy = XOpenDisplay(NULL);
    char ret[32];

    int i,j;
    while(1) {
        XQueryKeymap(dpy, ret);
        puts("keymap:");
        for (i=0; i<4; i++) {
            for (j=0; j<8; j++) {
                printf("%.2x ", ret[j + 8*i]);
            }
            putchar('\n');
        }
        if (BIT(ret, 24))
            break;
        sleep(1);
    }

    XCloseDisplay(dpy);
    return 0;
}
