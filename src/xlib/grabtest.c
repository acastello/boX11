#include <stdio.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>

int main(void)
{
    XEvent ret;
    XKeyEvent *kev;
    XKeyEvent kevo;
    Display *dpy = XOpenDisplay(NULL);
    Window root = DefaultRootWindow(dpy), 
           tar = 0x4400009;

    XGrabKey(dpy, 32, AnyModifier, root, 1, GrabModeAsync, GrabModeAsync);
    XGrabKey(dpy, 24, 0, root, 1, GrabModeSync, GrabModeSync);

    while (1) {
        XNextEvent(dpy, &ret);
        if (ret.type != KeyRelease && ret.type != KeyPress)
            continue;
        kev = (XKeyEvent *) &ret;

        printf(
                "type: %d\n"
                "keycode: %d\n"
                "state: 0x%x\n\n", kev->type, kev->keycode, kev->state); 

        kevo = *kev;
        kevo.window = tar;
        kevo.same_screen = True;
        kevo.keycode = 32;
        XSendEvent(dpy, tar, True, ret.type + 1, (XEvent *) &kevo);

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
    XUngrabKey(dpy, 24, 0, root);
    XCloseDisplay(dpy);
    
    return 0;
}
