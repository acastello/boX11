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

    dlopen(NULL, RTLD_GLOBAL | RTLD_NOW);
    ERR;

    handle = dlopen(so, RTLD_LAZY);
    ERR;

    GetCursorPos(NULL);
    ERR;

    hs_init = dlsym(handle, "hs_init");
    ERR;
    hs_exit = dlsym(handle, "hs_exit");

    run = dlsym(handle, "run");

    hs_init(&argc, &argv);
    run();
    hs_exit();

    dlclose(handle);

    return 0;
}
