#ifndef _BOX11_H
#define _BOX11_H

#define key_t __key_t
#include <windows.h>
#undef key_t


#define BOX_BYNAME  0
#define BOX_BYCLASS 1
#define BOX_CMP     0
#define BOX_REGEX   2

/*
 *  `key_t' contains a virtual key in its first 16 bits and
 *  a scan code in the upper 16
 */
typedef unsigned int vk_t, sc_t, key_t; 

#define KEY_TO_VK(c) (c & 0xffff)
#define KEY_TO_SC(c) ((c >> 16) & 0xffff)
#define TO_KEY(vk,sc) (sc << 16 | vk & 0xffff)

key_t fromVK(vk_t); 

// OR'd BOX_* flags 
HWND *getWins(int flags, char *filter);
HWND *getWinsBy(int (*filter_func)(HWND));
HWND getFocus(void);

char *getName(HWND);
char *getClass(HWND);

void postKey(key_t, HWND);
void postKeyDown(key_t, HWND); 
void postKeyUp(key_t, HWND); 
void postChar(wchar_t, HWND);
void postKeyChar(key_t, wchar_t, HWND);

void sendKey(key_t, HWND); 
void sendKeyDown(key_t, HWND); 
void sendKeyUp(key_t, HWND); 
void sendChar(wchar_t, HWND); 
void sendKeyChar(key_t, wchar_t, HWND);

// void postClick(vk_t, HWND);
void sendClick(vk_t, HWND);

void moveMouse(double xprop, double yprop, HWND);

void clickProp(int key, double xprop, double yprop, HWND);

void setText(char *txt, HWND);


#endif
