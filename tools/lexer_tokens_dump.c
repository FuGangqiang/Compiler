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

    FuLexer *l = FuLexer_new(pkg);
    FuLexer_for_file(l, fpath);

    FuStr *dump = FuLexer_dump(l);
    FuStr_print(stdout, dump);

    FuStr_drop(dump);
    FuLexer_drop(l);
    FuPkg_drop(pkg);
    return 0;
}
