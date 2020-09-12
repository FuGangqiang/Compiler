#include <assert.h>
#include <limits.h>
#include <unistd.h>

#include "unix.h"

FuStr *FuStr_abs_path(char *fpath) {
    char *rpath = realpath(fpath, NULL);
    FuStr *path = FuStr_from_utf8_cstr(rpath);
    free(rpath);
    return path;
}

FuStr *FuStr_rel_path(FuStr *real_path, FuStr *start) {
    if (FuStr_starts_with(real_path, start)) {
        fu_size_t start_len = FuStr_len(start);
        fu_size_t real_len = FuStr_len(real_path);
        if (FuStr_get_char(start, start_len - 1) != '/') {
            start_len++;
        }
        return FuStr_from_slice(real_path, start_len, real_len - start_len);
    }
    return FuStr_clone(real_path);
}

/* `path` must be real path */
FuStr *FuStr_path_dir(FuStr *path) {
    fu_size_t len = FuStr_len(path);
    assert(len > 0);
    assert(FuStr_get_char(path, 0) == '/');

    fu_size_t i = len - 1;
    if (FuStr_get_char(path, i) == '/') {
        i--;
    }
    while (i != 0) {
        if (FuStr_get_char(path, i) == '/') {
            break;
        }
        i--;
    }
    if (i == 0) {
        i = 1;
    }
    return FuStr_from_slice(path, 0, i);
}

void FuStr_path_join(FuStr *dir, FuStr *name) {
    if (FuStr_last_char(dir) != '/') {
        FuStr_push(dir, '/');
    }
    FuStr_append(dir, name);
}
