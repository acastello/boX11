#include <stdio.h>
#include <X11/Xlib.h>

#define BIT(arr, i) ((arr[i/8]>>(i%8))&0x01)

int main(void)
{
    char km[32];
    Display *dpy = XOpenDisplay(NULL);
    XQueryKeymap(dpy, km);

    XCloseDisplay(dpy);
    return 0;
}
