#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include <windows.h>
#include <string.h>

#define ERR \
    do { \
        err = dlerror(); \
        if (err) { \
            puts(err); \
            exit(1); \
        } \
    } while(0); \

void extend_env(void);


int main(int argc, char **argv)
{
    void *handle;
    void (*run)(void);
    void (*hs_init)(int *, char ***);
    void (*hs_exit)(void);
    char *err;

    char *so = argc > 1 ? argv[1] : "./Main.so";

    extend_env();
    puts(getenv("LD_LIBRARY_PATH"));

    dlopen(NULL, RTLD_GLOBAL | RTLD_NOW);
    ERR;

    handle = dlopen(so, RTLD_LAZY);
    ERR;

    GetCursorPos(NULL);
    ERR;

    hs_init = dlsym(handle, "hs_init");
    ERR;
    hs_exit = dlsym(handle, "hs_exit");

    run = dlsym(handle, "box_main");

    hs_init(&argc, &argv);
    run();
    hs_exit();

    dlclose(handle);

    return 0;
}

#define EXT_STR "LD_LIBRARY_PATH=."
char ext_path[4096] = EXT_STR;
char *ext_p = &ext_path[sizeof EXT_STR];
void extend_env(void)
{
    char *ld = getenv("LD_LIBRARY_PATH");

    if (ld) {
        *ext_p++ = ',';
        memcpy(ext_p, ld,
                strnlen(ld, 4096 - (ssize_t) ext_p + (ssize_t) ext_path));
    }
    putenv(ext_path);
}

