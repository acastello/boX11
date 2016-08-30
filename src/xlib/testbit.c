#include <stdio.h>

#define BIT(arr, i) ((arr[i/8]>>(i%8))&0x01)

int main(void)
{
    char a[2] = { 0, 0};
    char b[2] = { 1+4+8, 2};

    printf("0x%x\n", BIT(b,0));
    return 0;
}
