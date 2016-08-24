#include "boX11.h"

int main(void)
{
    messageBox("msg", "title", 0);
    sendKey((HWND) 196668, 'c', 'c');
    test((void *) 196668);
    return 0;
}
