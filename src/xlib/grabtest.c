#include <stdio.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>

int main(void)
{
    XEvent ret;
    XKeyEvent *kev;
    Display *dpy = XOpenDisplay(NULL);
    Window root = DefaultRootWindow(dpy);

    XGrabKey(dpy, 32, AnyModifier, root, 1, GrabModeAsync, GrabModeAsync);
    XGrabKey(dpy, 24, 0x10, root, 1, GrabModeAsync, GrabModeAsync);

    while (1) {
        XNextEvent(dpy, &ret);
        printf("type: %d\n", ret.type); 

        kev = (XKeyEvent *) &ret;
        switch (kev->keycode) {
            case 24:
                goto exit;
                break;
            default:
                break;
        }
    }

exit:
    XUngrabKey(dpy, 32, AnyModifier, root);
    XCloseDisplay(dpy);
    
    return 0;
}
