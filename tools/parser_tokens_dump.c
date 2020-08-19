#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "parse.h"

int main(int argc, char **argv) {
    if (argc != 2) {
        printf("usage: CMD fpath");
        exit(1);
    }
    FuContext *ctx = FuContext_new();
    FuContext_init(ctx);
    FuParser *p = FuParser_new(ctx);
    FuParser_for_file(p, argv[1], strlen(argv[1]));
    FuStr *dump = FuParser_dump_tokens(p);
    FuStr_print(stdout, dump);

    FuStr_drop(dump);
    FuParser_drop(p);
    FuContext_drop(ctx);
    return 0;
}
