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

    FuLexer *l = FuLexer_new(ctx);
    FuLexer_for_file(l, fpath);

    FuStr *dump = FuLexer_dump(l);
    FuStr_print(stdout, dump);

    FuStr_drop(dump);
    FuLexer_drop(l);
    FuCtx_drop(ctx);
    return 0;
}
