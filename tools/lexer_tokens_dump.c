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
    FuLexer *l = FuLexer_new(ctx);
    FuLexer_for_file(l, argv[1], strlen(argv[1]));
    FuStr *dump = FuLexer_dump(l);
    FuStr_print(stdout, dump);

    FuStr_drop(dump);
    FuLexer_drop(l);
    FuCtx_drop(ctx);
    return 0;
}
