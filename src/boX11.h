#include <windows.h>


#define BOX_BYNAME  0
#define BOX_BYCLASS 1
#define BOX_CMP     0
#define BOX_REGEX   2

HWND *getWins(char *filter, int flags);
#define getWinsByClass(filter) getWins(filter, BOX_BYCLASS)
#define getWinsByClassEx(filter) getWins(filter, BOX_BYCLASS | BOX_REGEX)
#define getWinsByName(filter) getWins(filter, BOX_BYNAME)
#define getWinsByNameEx(filter) getWins(filter, BOX_BYNAME | BOX_REGEX)

HWND *getWinsBy(int (*filter_fun)(HWND));

