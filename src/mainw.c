#include "boX11.h"

int main(void)
{
    messageBox("msg", "title", 0);
    sendKey('c', (HWND) 196668);
    return 0;
}
