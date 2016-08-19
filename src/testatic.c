#include <stdio.h>

typedef struct {
    int i;
} i_t;

i_t *f(void)
{
    static i_t tmp = {.i = 1};
    tmp.i++;

    return &tmp;
}

int main(void)
{
    printf("1: %d\n", f()->i);
    printf("2: %d\n", f()->i);
    printf("3: %d\n", f()->i);
    printf("4: %d\n", f()->i);

    return 0;
}

