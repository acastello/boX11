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
           tar = 0x240000a;

    XGrabKey(dpy, 24, 0, root, 1, GrabModeAsync, GrabModeAsync);
    XGrabKeyboard(dpy, root, False, GrabModeAsync, GrabModeAsync, 0);

    while (1) {
        XNextEvent(dpy, &ret);
        if (ret.type != KeyRelease && ret.type != KeyPress)
            continue;
        kev = (XKeyEvent *) &ret;
        kevo = * (XKeyEvent *) &ret;

        printf(
                "type: %d\n"
                "window: %p\n"
                "subwindow: %d\n"
                "keycode: %d\n"
                "state: 0x%x\n\n", kev->type, kev->window, kev->subwindow, kev->keycode, kev->state); 

        kevo = *kev;
        // kevo.window = tar;
        // kevo.same_screen = True;
        // kevo.keycode = 32;
        // XSendEvent(dpy, tar, True, ret.type + 1, (XEvent *) &kevo);

        kev->window = kev->subwindow;
        switch (kev->keycode) {
            case 24:
                goto exit;
                break;
            default:
                XSendEvent(dpy, kev->window, True, ret.type+1, (XEvent *) kev);
                break;
        }
    }

exit:
    XUngrabKey(dpy, 24, 0, root);
    XCloseDisplay(dpy);
    
    return 0;
}

