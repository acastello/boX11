#include <stdio.h>
#include <regex.h>
#include <unistd.h>

#include <sched.h>

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

HWND *getWins(int flags, char *filter)
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
inline void sendKey(char vk, HWND hwnd)
{
    SendMessage(hwnd, WM_KEYDOWN, vk, 0);
    SendMessage(hwnd, WM_KEYUP, vk, 0);
}

/*
 *		sendKeyDown
 */
inline void sendKeyDown(char vk, HWND hwnd)
{
    SendMessage(hwnd, WM_KEYDOWN, vk, 0);
}

/*
 * 		sendKeyUp
 */
inline void sendKeyUp(char vk, HWND hwnd)
{
    SendMessage(hwnd, WM_KEYUP, vk, 0);
}

/*******************************************************************************
 *		sendChar
 */
inline void sendChar(char ch, HWND hwnd)
{
    SendMessage(hwnd, WM_CHAR, ch, 0);
}

/*
 *      sendKeyChar
 */
inline void sendKeyChar(char vk, char ch, HWND hwnd)
{
    SendMessage(hwnd, WM_KEYDOWN, vk, 0);
    SendMessage(hwnd, WM_CHAR, ch, 0);
    SendMessage(hwnd, WM_KEYUP, vk, 0);
}

/*
 *      sendClick
 */
inline void sendClick(int k, HWND hwnd)
{
    switch (k) {
        case 1:
            SendMessage(hwnd, WM_LBUTTONDOWN, 0, 0);
            SendMessage(hwnd, WM_LBUTTONUP, 0, 0);
            break;
        case 2:
            SendMessage(hwnd, WM_MBUTTONDOWN, 0, 0);
            SendMessage(hwnd, WM_MBUTTONUP, 0, 0);
            break;
        case 3:
            SendMessage(hwnd, WM_RBUTTONDOWN, 0, 0);
            SendMessage(hwnd, WM_RBUTTONUP, 0, 0);
            break;
        case 4:
            SendMessage(hwnd, WM_XBUTTONDOWN, 1<<16, 0);
            SendMessage(hwnd, WM_XBUTTONUP,   1<<16, 0);
            break;
        case 5:
            SendMessage(hwnd, WM_XBUTTONDOWN, 2<<16, 0);
            SendMessage(hwnd, WM_XBUTTONUP,   2<<16, 0);
            break;
        case 6:
            SendMessage(hwnd, WM_MOUSEWHEEL, 1<<16, 0);
            break;
        case 7:
            SendMessage(hwnd, WM_MOUSEWHEEL, (-1)<<16, 0);
            break;
        case 8:
            SendMessage(hwnd, WM_MOUSEHWHEEL, (1)<<16, 0);
            break;
        case 9:
            SendMessage(hwnd, WM_MOUSEWHHEEL, (-1)<<16, 0);
            break;
        default:
            break;
    }
}

/*
 *      moveMouse
 */
inline void moveMouse(double xp, double yp, HWND hwnd)
{
    RECT clir;
    POINT p;

    GetClientRect(hwnd, &clir);
    p.x = xp*clir.right;
    p.y = yp*clir.bottom;
    ClientToScreen(hwnd, &p);
    SetCursorPos(p.x, p.y);
}

/*
 *      moveMouseAbs
 */

/*
 *      clickProp
 */
inline void clickProp(int k, double xp, double yp, HWND hwnd)
{
    moveMouse(xp, yp, hwnd);
    Sleep(50);
    sendClick(k, hwnd);
    Sleep(20);
}

/*******************************************************************************
  *		setText
  */
inline void setText(char *txt, HWND hwnd)
{
    SendMessage(hwnd, WM_SETTEXT, 0, (ssize_t) txt);
}

/*******************************************************************************
 *		getName
 */
inline char *getName(HWND hwnd)
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

/*******************************************************************************
 *		test
 */
void test(HWND hwnd)
{
    static int i = 0;
    int j = i++;
    printf("starting  %d\n", j);
    getchar();
    // SendMessage(hwnd, WM_KEYDOWN, 90, 0);
    // sync();
    // sched_yield();
    printf("finishing %d\n", j);
}
