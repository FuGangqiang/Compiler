#include <assert.h>
#include <stdarg.h>

#include "alloc.h"
#include "error.h"
#include "parser.h"

FuParser *FuParser_new(FuContext *ctx) {
    FuParser *p = FuMem_new(FuParser);
    p->ctx = ctx;
    p->tok_buf = FuVec_new(sizeof(FuToken));
    p->cursor = 0;
    p->unclosed_delims = FuVec_new(sizeof(FuToken));
    return p;
}

void FuParser_drop(FuParser *p) {
    if (!p) {
        return;
    }
    FuVec_drop(p->unclosed_delims);
    FuVec_drop(p->tok_buf);
    FuLexer_drop(p->lexer);
    FuMem_free(p);
}

void FuParser_for_file(FuParser *p, char *fpath, fu_size_t len) {
    p->lexer = FuLexer_new(p->ctx);
    FuLexer_for_file(p->lexer, fpath, len);
    FuToken tok = FuLexer_get_token(p->lexer);
    FuVec_push(p->tok_buf, &tok);
}
