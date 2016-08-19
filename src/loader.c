#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include <windows.h>

#define ERR \
    do { \
        err = dlerror(); \
        if (err) { \
            puts(err); \
            exit(1); \
        } \
    } while(0); \



int main(int argc, char **argv)
{
    void *handle;
    void (*run)(void);
    void (*hs_init)(int *, char ***);
    void (*hs_exit)(void);
    char *err;

    char *so = argc > 1 ? argv[1] : "./run.so";
    handle = dlopen(so, RTLD_LAZY);
    ERR;

    run = dlsym(handle, "run");
    ERR;

    hs_init = dlsym(handle, "hs_init");
    ERR;
    hs_exit = dlsym(handle, "hs_exit");

    hs_init(&argc, &argv);
    GetCursorPos(NULL);
    run();
    hs_exit();

    dlclose(handle);

    return 0;
}
