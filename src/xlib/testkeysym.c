#include <X11/Xlib.h>
#include <stdio.h>

int main(void)
{
    Display *dpy = XOpenDisplay(NULL);
    printf("ks %d -> kc %d\n", 'u', XKeysymToKeycode(dpy, 'u'));
    printf("str %s -> ks %d\n", "minus", XStringToKeysym("minus"));

    XCloseDisplay(dpy);
    return 0;
}
