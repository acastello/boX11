#ifndef _BOX11_H
#define _BOX11_H

#include <windows.h>


#define BOX_BYNAME  0
#define BOX_BYCLASS 1
#define BOX_CMP     0
#define BOX_REGEX   2

HWND *getWins(int flags, char *filter);
#define getWinsByClass(filter) getWins(filter, BOX_BYCLASS)
#define getWinsByClassEx(filter) getWins(filter, BOX_BYCLASS | BOX_REGEX)
#define getWinsByName(filter) getWins(filter, BOX_BYNAME)
#define getWinsByNameEx(filter) getWins(filter, BOX_BYNAME | BOX_REGEX)

HWND *getWinsBy(int (*filter_func)(HWND));

int messageBox(char *body, char *title, int flags);

void postKey(int vk, HWND hwnd);

void postKeyDown(int vk, HWND hwnd);

void postKeyUp(int vk, HWND hwnd);

void sendKey(int vk, HWND hwnd);

void sendKeyDown(int vk, HWND hwnd);

void sendKeyUp(int vk, HWND hwnd);

void sendChar(char ch, HWND hwnd);

void sendKeyChar(int vk, char ch, HWND hwnd);

void sendClick(int k, HWND hwnd);

void moveMouse(double xprop, double yprop, HWND target);

void clickProp(int key, double xprop, double yprop, HWND target);

void setText(char *txt, HWND hwnd);

char *getName(HWND hwnd);

char *getClass(HWND hwnd);

void test(HWND);

#endif
