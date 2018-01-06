#include "boX11.h"

#include <stdio.h>
#include <regex.h>
#include <unistd.h>

#include <sched.h>

#define KEYDOWN(hwnd, key) hwnd, WM_KEYDOWN, KEY_TO_VK(key), \
    1 | (key & 0xff0000)

#define KEYUP(hwnd, key) hwnd, WM_KEYUP, KEY_TO_VK(key), \
    1 | (1 << 30) | (1 << 31) | (key & 0xff0000)

#define XY_TO_BITS(x,y) (( (((short) y) << 16) | (((short) x) & 0xffff) ))

#define BETWEEN(min,x,max) (( (min) > (x) ? (min) : (max) < (x) ? (max) : (x) ))

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

/*
 *      getForeground
 */
HWND getForeground(void)
{
    return GetForegroundWindow();
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
inline void postKey(key_t key, HWND hwnd)
{
    PostMessage(KEYDOWN(hwnd,key));
    PostMessage(KEYUP(hwnd,key));
}

/*
 *      postKeyDown
 */
inline void postKeyDown(key_t key, HWND hwnd)
{
    PostMessage(KEYDOWN(hwnd, key));
}

/*
 *      postKeyUp
 */
inline void postKeyUp(key_t key, HWND hwnd)
{
    PostMessage(KEYUP(hwnd, key));
}

/*
 *      postChar
 */
inline void postChar(wchar_t ch, HWND hwnd)
{
    PostMessage(hwnd, WM_CHAR, ch, 0);
}

/*
 *      postKeyChar
 */
inline void postKeyChar(key_t key, wchar_t ch, HWND hwnd)
{
    PostMessage(KEYDOWN(hwnd, key));
    PostMessage(hwnd, WM_CHAR, ch, 1 | (key & 0xff0000) | (1 << 30));
    PostMessage(KEYUP(hwnd, key));
}

/*******************************************************************************
 *		sendKey
 */
inline void sendKey(key_t key, HWND hwnd)
{
    SendMessage(KEYDOWN(hwnd, key));
    SendMessage(KEYUP(hwnd, key));
}

/*
 *		sendKeyDown
 */
inline void sendKeyDown(key_t key, HWND hwnd)
{
    SendMessage(KEYDOWN(hwnd, key));
}

/*
 * 		sendKeyUp
 */
inline void sendKeyUp(key_t key, HWND hwnd)
{
    SendMessage(KEYUP(hwnd, key));
}

/*******************************************************************************
 *		sendChar
 */
inline void sendChar(wchar_t ch, HWND hwnd)
{
    SendMessage(hwnd, WM_CHAR, ch, 0);
}

/*
 *      sendKeyChar
 */
inline void sendKeyChar(key_t key, wchar_t ch, HWND hwnd)
{
    SendMessage(KEYDOWN(hwnd, key));
    SendMessage(hwnd, WM_CHAR, ch, 0);
    SendMessage(KEYUP(hwnd, key));
}

/*
 *      sendClick
 */
inline void sendClick(vk_t k, HWND hwnd)
{
    sendClickAt(k, hwnd, 0, 0);
}

/*
 *      sendClickAt
 */
void sendClickAt(vk_t k, HWND hwnd, short x, short y)
{
    LPARAM pos = XY_TO_BITS(x,y);
    #define sendClick_delay 0
    switch (k) {
        case 1:
            SendMessage(hwnd, WM_LBUTTONDOWN, 0, pos);
            #if sendClick_delay > 0
                Sleep(sendClick_delay);
            #endif
            SendMessage(hwnd, WM_LBUTTONUP, 0, pos);
            break;
        case 2:
            SendMessage(hwnd, WM_MBUTTONDOWN, 0, pos);
            #if sendClick_delay > 0
                Sleep(sendClick_delay);
            #endif
            SendMessage(hwnd, WM_MBUTTONUP, 0, pos);
            break;
        case 3:
            SendMessage(hwnd, WM_RBUTTONDOWN, 0, pos);
            #if sendClick_delay > 0
                Sleep(sendClick_delay);
            #endif
            SendMessage(hwnd, WM_RBUTTONUP, 0, pos);
            break;
        case 4:
            SendMessage(hwnd, WM_XBUTTONDOWN, 1<<16, pos);
            #if sendClick_delay > 0
                Sleep(sendClick_delay);
            #endif
            SendMessage(hwnd, WM_XBUTTONUP,   1<<16, pos);
            break;
        case 5:
            SendMessage(hwnd, WM_XBUTTONDOWN, 2<<16, pos);
            #if sendClick_delay > 0
                Sleep(sendClick_delay);
            #endif
            SendMessage(hwnd, WM_XBUTTONUP,   2<<16, pos);
            break;
        case 6:
            SendMessage(hwnd, WM_MOUSEWHEEL, 1<<16, pos);
            break;
        case 7:
            SendMessage(hwnd, WM_MOUSEWHEEL, (-1)<<16, pos);
            break;
        case 8:
            SendMessage(hwnd, WM_MOUSEWHEEL, (1)<<16, pos);
            break;
        case 9:
            SendMessage(hwnd, WM_MOUSEWHEEL, (-1)<<16, pos);
            break;
        default:
            break;
    }
}

/*
 *      moveMouse
 */
POINT moveMouse(double xp, double yp, HWND hwnd)
{
    return moveMouseLin(0, xp, 0, yp, hwnd);
}

/*
 *      moveMouseAbs
 */
POINT moveMouseAbs(int x, int y, HWND hwnd)
{
    return moveMouseLin(x, 0.0, y, 0.0, hwnd);
}

/*
 *      moveMouseLin
 */
POINT moveMouseLin(int x, double xp, int y, double yp, HWND hwnd)
{
    RECT clir;
    POINT p;
    int calcx, calcy;

    GetClientRect(hwnd, &clir);
    calcx = x + xp * clir.right;
    calcy = y + yp * clir.bottom;
    p.x = BETWEEN(0, calcx, clir.right-1);
    p.y = BETWEEN(0, calcy, clir.bottom-1);
    ClientToScreen(hwnd, &p);
    // printf("[%p] (%d, %f, %d, %f) = (%d, %d)", hwnd, x, xp, y, yp, p.x, p.y);
    SetCursorPos(p.x, p.y);
    Sleep(1);
    SendMessage(hwnd, WM_MOUSEMOVE, 0, XY_TO_BITS(p.x, p.y));
    Sleep(1);
    SendMessage(hwnd, WM_MOUSEMOVE, 0, XY_TO_BITS(p.x, p.y));
    return p;
}

/*
 *      click
 */
inline void click(int k, double xp, double yp, HWND hwnd)
{
    clickLin(k, 0, xp, 0, yp, hwnd);
}

/*
 *      clickAbs
 */
inline void clickAbs(int k, int x, int y, HWND hwnd)
{
    clickLin(k, x, 0.0, y, 0.0, hwnd);
}

/*
 *      clickLin
 */
inline void clickLin(int k, int x, double xp, int y, double yp, HWND hwnd)
{
    POINT p = moveMouseLin(x, xp, y, yp, hwnd);
    sendClickAt(k, hwnd, p.x, p.y);
}

/*
 *      getPixel
 */
DWORD getPixel(HWND hwnd, int x, int y)
{
    HDC hdc;
    POINT p;

    hdc = GetDC(hwnd);

    if (!hwnd) {
        return GetPixel(hdc, x, y);
    } else {
        p.x = x;
        p.y = y;
        ClientToScreen(hwnd, &p);
        return GetPixel(hdc, p.x, p.y);
    }
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

key_t fromVK(vk_t vk)
{
    return TO_KEY(vk, MapVirtualKey(vk, 0));
}

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
