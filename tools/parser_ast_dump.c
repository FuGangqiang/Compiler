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
    FuStr *pkg_dir = FuStr_path_dir(fpath);
    FuCtx *ctx = FuCtx_new(pkg_dir);
    FuCtx_init(ctx);

    FuParser *p = FuParser_new(ctx);
    p->cur_dir = FuStr_path_dir(fpath);
    FuParser_for_file(p, fpath);

    FuNode *pkg = FuParser_parse_pkg(p);
    FuStr *dump = FuNode_display(pkg, 0);
    FuStr_print(stdout, dump);

    FuStr_drop(dump);
    FuParser_drop(p);
    FuCtx_drop(ctx);
    return 0;
}
