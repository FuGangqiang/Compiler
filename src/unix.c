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

FuStr *FuStr_path_fname(FuStr *path) {
    fu_size_t len = FuStr_len(path);
    assert(len > 0);

    if (FuStr_last_char(path) == '/') {
        fprintf(stdout, "file path can not be a dir: ");
        FuStr_print(path, stderr);
        fprintf(stdout, "\n");
        exit(1);
    }

    fu_size_t i;
    if (FuStr_rfind(path, '/', &i)) {
        return FuStr_from_slice(path, i, len);
    }
    return FuStr_clone(path);
}

FuStr *FuStr_path_file_stem(FuStr *path) {
    fu_size_t len = FuStr_len(path);
    assert(len > 0);

    if (FuStr_last_char(path) == '/') {
        fprintf(stdout, "file path can not be a dir: ");
        FuStr_print(path, stderr);
        fprintf(stdout, "\n");
        exit(1);
    }

    fu_bool_t found = FU_FALSE;
    fu_size_t i;
    for (i = len; i > 0; i--) {
        FuChar fc = FuStr_get_char(path, i - 1);
        if (fc == '.') {
            found = FU_TRUE;
            break;
        }
        if (fc == '/') {
            break;
        }
    }

    FuStr *new;
    if (i == 0 || !found) {
        new = FuStr_clone(path);
    } else {
        new = FuStr_from_slice(path, 0, i - 1);
    }
    return new;
}

FuStr *FuStr_path_with_extension(FuStr *path, char *extension) {
    fu_size_t len = FuStr_len(path);
    assert(len > 0);

    if (FuStr_last_char(path) == '/') {
        fprintf(stdout, "file path can not be a dir: ");
        FuStr_print(path, stderr);
        fprintf(stdout, "\n");
        exit(1);
    }

    fu_bool_t found = FU_FALSE;
    fu_size_t i;
    for (i = len; i > 0; i--) {
        FuChar fc = FuStr_get_char(path, i - 1);
        if (fc == '.') {
            found = FU_TRUE;
            break;
        }
        if (fc == '/') {
            break;
        }
    }

    FuStr *new;
    if (i == 0 || !found) {
        new = FuStr_clone(path);
        FuStr_push(path, '.');
    } else {
        /* found */
        new = FuStr_from_slice(path, 0, i);
    }
    FuStr_push_utf8_cstr(new, extension);
    return new;
}

FuStr *FuStr_get_cur_dir() {
    char path[4096];
    if (!getcwd(path, 4096)) {
        fprintf(stderr, "Error: getcwd error");
        exit(1);
    }
    FuStr *str = FuStr_new();
    FuStr_push_utf8_cstr(str, path);
    return str;
}

void FuStr_path_join(FuStr *dir, FuStr *name) {
    if (FuStr_last_char(dir) != '/') {
        FuStr_push(dir, '/');
    }
    FuStr_append(dir, name);
}
