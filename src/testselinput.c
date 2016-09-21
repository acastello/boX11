#include <stdio.h>

#include <X11/Xlib.h>

int main(int argc, char **argv)
{
    Window w = 0;
    XEvent ev;
    Display *dpy = XOpenDisplay(NULL);

    if (argc > 1) {
        sscanf(argv[1], "0x%x\n", &w);
        if (!w)
            sscanf(argv[1], "%d\n", &w);
    }

    if (!w)
        w = DefaultRootWindow(dpy);

    printf("filtering window: 0x%x\n", w);

    XSelectInput(dpy, w, KeyReleaseMask);

    while (1) {
        XNextEvent(dpy, &ev);
        printf("type: %d\n", ev.type);
    }

    XCloseDisplay(dpy);
    return 0;
}
