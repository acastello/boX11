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

/*
 *  `key_t' contains a virtual key in its first 16 bits and
 *  a scan code in the upper 16
 */

typedef unsigned int vk_t, sc_t, key_t;

#define KEY_TO_VK(c) (c & 0xffff)
#define KEY_TO_SC(c) ((c >> 16) & 0xffff)
#define TO_KEY(vk,sc) (sc << 16 | vk & 0xffff)

HWND *getWinsBy(int (*filter_func)(HWND));

int messageBox(char *body, char *title, int flags);

Nigger_t

void postKey(key_t, HWND);

void postKeyDown(key_t, HWND);

void postKeyUp(key_t, HWND);

void sendKey(key_t, HWND);

void sendKeyDown(key_t, HWND);

void sendKeyUp(key_t, HWND);

void sendChar(char ch, HWND);

void sendKeyChar(key_t, char ch, HWND);

void sendClick(key_t, HWND);

void moveMouse(double xprop, double yprop, HWND);

void clickProp(int key, double xprop, double yprop, HWND);

void setText(char *txt, HWND);

char *getName(HWND);

char *getClass(HWND);

int fromVK(int vk);

void test(HWND);

#endif
