#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "parse.h"
#include "unix.h"

int main(int argc, char **argv) {
    if (argc != 2) {
        printf("usage: CMD fpath");
        exit(1);
    }

    FuStr *fpath = FuStr_abs_path(argv[1]);
    FuStr *dir = FuStr_path_dir(fpath);
    FuPkg *pkg = FuPkg_new(dir);
    FuPkg_init(pkg);

    FuParser *p = FuParser_new(pkg);
    p->cur_dir = FuStr_path_dir(fpath);
    FuParser_for_file(p, fpath);

    FuParser_parse_pkg(p);

    FuStr *dump = FuPkg_display(pkg, 0);
    FuStr_print(stdout, dump);

    FuStr_drop(dump);
    FuParser_drop(p);
    FuPkg_drop(pkg);
    return 0;
}
