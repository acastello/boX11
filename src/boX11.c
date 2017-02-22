#include <stdio.h>
#include <regex.h>
#include <unistd.h>

#include <sched.h>

#include "boX11.h"

#define KEYDOWN(hwnd, vk) hwnd, WM_KEYDOWN, vk, \
    1 | (MapVirtualKey(vk,0) << 16)

#define KEYUP(hwnd, vk) hwnd, WM_KEYUP, vk, \
    1 | (1 << 31) | (MapVirtualKey(vk,0) << 16)

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

/*
 *      getFocus
 */
HWND getFocus(void)
{
    return GetFocus();
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

/*
 *      postKey
 */
inline void postKey(int vk, HWND hwnd)
{
    PostMessage(KEYDOWN(hwnd,vk));
    PostMessage(KEYUP(hwnd,vk));
}

/*
 *      postKeyDown
 */
inline void postKeyDown(int vk, HWND hwnd)
{
    PostMessage(KEYDOWN(hwnd, vk));
}

/*
 *      postKeyUp
 */
inline void postKeyUp(int vk, HWND hwnd)
{
    PostMessage(KEYUP(hwnd,vk));
}

/*******************************************************************************
 *		sendKey
 */
inline void sendKey(int vk, HWND hwnd)
{
    SendMessage(KEYDOWN(hwnd, vk));
    SendMessage(KEYUP(hwnd, vk));
}

/*
 *		sendKeyDown
 */
inline void sendKeyDown(int vk, HWND hwnd)
{
    SendMessage(KEYDOWN(hwnd, vk));
}

/*
 * 		sendKeyUp
 */
inline void sendKeyUp(int vk, HWND hwnd)
{
    SendMessage(KEYUP(hwnd, vk));
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
inline void sendKeyChar(int vk, char ch, HWND hwnd)
{
    SendMessage(KEYDOWN(hwnd, vk));
    SendMessage(hwnd, WM_CHAR, ch, 0);
    SendMessage(KEYUP(hwnd, vk));
}

/*
 *      sendClick
 */
inline void sendClick(int k, HWND hwnd)
{
    #define sendClick_delay 0
    switch (k) {
        case 1:
            SendMessage(hwnd, WM_LBUTTONDOWN, 0, 0);
            #if sendClick_delay > 0
                Sleep(sendClick_delay);
            #endif
            SendMessage(hwnd, WM_LBUTTONUP, 0, 0);
            break;
        case 2:
            SendMessage(hwnd, WM_MBUTTONDOWN, 0, 0);
            #if sendClick_delay > 0
                Sleep(sendClick_delay);
            #endif
            SendMessage(hwnd, WM_MBUTTONUP, 0, 0);
            break;
        case 3:
            SendMessage(hwnd, WM_RBUTTONDOWN, 0, 0);
            #if sendClick_delay > 0
                Sleep(sendClick_delay);
            #endif
            SendMessage(hwnd, WM_RBUTTONUP, 0, 0);
            break;
        case 4:
            SendMessage(hwnd, WM_XBUTTONDOWN, 1<<16, 0);
            #if sendClick_delay > 0
                Sleep(sendClick_delay);
            #endif
            SendMessage(hwnd, WM_XBUTTONUP,   1<<16, 0);
            break;
        case 5:
            SendMessage(hwnd, WM_XBUTTONDOWN, 2<<16, 0);
            #if sendClick_delay > 0
                Sleep(sendClick_delay);
            #endif
            SendMessage(hwnd, WM_XBUTTONUP,   2<<16, 0);
            break;
        case 6:
            SendMessage(hwnd, WM_MOUSEWHEEL, 1<<16, 0);
            break;
        case 7:
            SendMessage(hwnd, WM_MOUSEWHEEL, (-1)<<16, 0);
            break;
        case 8:
            SendMessage(hwnd, WM_MOUSEWHEEL, (1)<<16, 0);
            break;
        case 9:
            SendMessage(hwnd, WM_MOUSEWHEEL, (-1)<<16, 0);
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
    Sleep(70);
    sendClick(k, hwnd);
    Sleep(70);
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

void focusWin(HWND hwnd)
{
    SetForegroundWindow(hwnd);
}

int fromVK(int vk)
{
    return MapVirtualKey(vk, 0);

/*
 *		processIdOfWindow
 */
DWORD processIdOfWindow(HWND hwnd)
{
    DWORD ret;
    GetWindowThreadProcessId(hwnd, &ret);
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

/*
 *		loadLibrary
 */
HMODULE loadLibrary(char *dllname)
{
    return LoadLibraryA(dllname);
}

/*
 *		getProcAddress
 */
void *getProcAddress(HMODULE module, char *funcname)
{
    return GetProcAddress(module, funcname);
}
