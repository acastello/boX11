#include <stdio.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>

int main(void)
{
    printf("%p\n", XGrabKey);
    return 0;
}
