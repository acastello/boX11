#include <sys/time.h>
#include <stdio.h>

int main(void)
{
    struct timeval tv;
    long long l;
    gettimeofday(&tv,NULL);
    l = tv.tv_sec *1000000 + tv.tv_usec;
    printf("usec: %lld\n", l);

    return 0;
}
