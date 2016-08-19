#include <stdio.h>
#include <regex.h>

#include "boX11.h"

#define NWINS 64

typedef struct {
    HWND wins[NWINS+1];
    int i;
    char *filter;
    int flags;
} getwins_t;

BOOL CALLBACK _getwins_cb(HWND, LPARAM);

HWND *getWins(char *filter, int flags)
{ 
    static getwins_t ret;

    ret.i = 0;
    ret.filter = filter;
    ret.flags = flags;

    EnumWindows(_getwins_cb, (size_t) &ret);
    ret.wins[ret.i] = NULL;
    // printf("%x -> %d\n", ret.wins, *ret.wins);

    return (HWND *) ret.wins;
}

BOOL CALLBACK _getwins_cb(HWND hwnd, LPARAM lparam)
{
    static char buff[256];
    getwins_t *sto;
    regex_t regex;
    
    sto = (getwins_t *) lparam;

    if (sto->i >= NWINS) {
        sto->wins[sto->i] = NULL;
        return FALSE;
    }

    if (!hwnd)
        return TRUE;

    if (sto->flags & BOX_BYCLASS)
        GetClassName(hwnd, buff, sizeof(buff)-1);
    else
        GetWindowText(hwnd, buff, sizeof(buff)-1);

    // printf("[  ] %d -> %s\n", hwnd, buff);

    if (sto->flags & BOX_REGEX) {
        if (!regcomp(&regex, sto->filter, REG_NOSUB) && 
                !regexec(&regex, buff, sizeof(buff)-1, NULL, 0)) {
            sto->wins[sto->i++] = hwnd;
            // printf("[OK] %d -> %s\n", hwnd, buff);
        }
    }

    else if (!strncmp(buff, sto->filter, sizeof(buff)-1)) {
        sto->wins[sto->i++] = hwnd;
        // printf("[OK] %d -> %s\n", hwnd, buff);
    }

    return TRUE;
} 
