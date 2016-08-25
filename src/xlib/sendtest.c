#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>

int main(int argc, char **argv)
{
    Display *dpy;
    Window root;
    Window tar;
    XKeyEvent kev;

    if (argc > 1)
        tar = strtol(argv[1], NULL, 16);
    if (argc < 1 || !tar)
        return 0;

    dpy = XOpenDisplay(NULL);
    root = DefaultRootWindow(dpy); 
    bzero(&kev, sizeof(kev));

    kev.type = KeyPress;
    kev.send_event = True;
    kev.keycode = 24;
    kev.root = root;
    kev.window = tar;
    kev.same_screen = True;

    printf("sending to window: 0x%x\n", tar);
    XSendEvent(dpy, tar, True, KeyPressMask, (XEvent *) &kev);

    XCloseDisplay(dpy);
    return 0;
}
