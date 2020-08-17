#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "parse.h"

int main(int argc, char **argv) {
    if (argc != 2) {
        printf("usage: CMD fpath");
        exit(1);
    }
    FuCtx *ctx = FuCtx_new();
    FuCtx_init(ctx);

    FuParser *p = FuParser_new(ctx);
    FuParser_for_file(p, argv[1], strlen(argv[1]));

    FuNode *pkg = FuParser_parse_pkg(p);
    FuStr *dump = FuNode_display(pkg, 0);
    FuStr_print(stdout, dump);

    FuStr_drop(dump);
    FuParser_drop(p);
    FuCtx_drop(ctx);
    return 0;
}
