#include <stdio.h>
#include <regex.h>

#include "boX11.h"

#define NWINS 64

/*******************************************************************************
 *		getWins
 */
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

/*******************************************************************************
 *		getWinsBy
 */
typedef struct {
    HWND wins[NWINS+1];
    int i;
    int (*filter_fun)(HWND);
} getwinsby_t;

BOOL CALLBACK _getwinsby_cb(HWND, LPARAM);
HWND *getWinsBy(int (*filter_fun)(HWND))
{
    static getwinsby_t ret;

    ret.i = 0;
    ret.filter_fun = filter_fun;

    EnumWindows(_getwinsby_cb, (ssize_t) &ret);
    ret.wins[ret.i] = NULL;

    return ret.wins;
}

BOOL CALLBACK _getwinsby_cb(HWND hwnd, LPARAM lparam)
{
    getwinsby_t *sto = (getwinsby_t *) lparam;

    if (sto->i >= NWINS) {
        sto->wins[sto->i] = NULL;
        return FALSE;
    }

    if (sto->filter_fun(hwnd)) {
        sto->wins[sto->i++] = hwnd;
    }

    return TRUE;
}

/*******************************************************************************
 *		getCursorPos
 */
UINT64 getCursorPos(void)
{
    POINT p;
    INT64 ret;

    GetCursorPos(&p);

    ret = p.x | ( ((UINT64) p.y) << 32);

    return ret;
}

/*******************************************************************************
 *		messageBox
 */
int messageBox(char *body, char *title, int flags)
{
    return MessageBoxA(NULL, body, title, flags);
}

/*******************************************************************************
 *		sendKey
 */
void sendKey(HWND hwnd, char vk, char ch)
{
    SendMessage(hwnd, WM_KEYDOWN, vk, 0);
    SendMessage(hwnd, WM_CHAR, ch, 0);
    SendMessage(hwnd, WM_KEYUP, vk, 0);
}

/*******************************************************************************
 *		getName
 */
char *getName(HWND hwnd)
{
    static char ret[1024];
    GetWindowText(hwnd, ret, sizeof(ret));
    return ret;
}

/*******************************************************************************
 *		getClass
 */
char *getClass(HWND hwnd)
{
    static char ret[1024];
    GetClassName(hwnd, ret, sizeof(ret));
    return ret;
}
