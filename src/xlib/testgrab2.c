#include <stdio.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>

int main(void)
{
    XEvent ret;
    XKeyEvent kev;
    Display *dpy = XOpenDisplay(NULL);
    Window root = DefaultRootWindow(dpy), 
           tar = 0x1200009;
    int rev;

    XGrabKey(dpy, 32, AnyModifier, root, False, GrabModeAsync, GrabModeAsync);
    XGrabKey(dpy, 24, 0, root, 1, GrabModeSync, GrabModeSync);

    while (1) {
        XNextEvent(dpy, &ret);
        if (ret.type != KeyRelease && ret.type != KeyPress) {
            printf("type: %d\n", ret.type);
            continue;
        }
        kev = *(XKeyEvent *) &ret;

        printf(
                "type: %d\n"
                "keycode: %d\n"
                "state: 0x%x\n\n", kev.type, kev.keycode, kev.state); 

        switch (kev.keycode) {
            case 24:
                goto exit;
            default:
                XGetInputFocus(dpy, &kev.window, &rev);
                XSendEvent(dpy, kev.window, False, ret.type + 1, (XEvent *) &kev);

                break;
        }
    }

exit:
    XUngrabKey(dpy, 32, AnyModifier, root);
    XUngrabKey(dpy, 24, 0, root);
    XCloseDisplay(dpy);
    
    return 0;
}
