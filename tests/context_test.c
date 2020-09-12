#include <assert.h>
#include <stdio.h>

#include "parse.h"
#include "str.h"

void test_symbol(FuCtx *ctx) {
    FuStr *s1 = FuStr_from_utf8_cstr("hello");
    FuStr *s2 = FuStr_from_utf8_cstr("hello");
    FuStr *s3 = FuStr_from_utf8_cstr("world");
    fu_sym_t sym1 = FuCtx_intern_symbol(ctx, FuStr_clone(s1));
    fu_sym_t sym2 = FuCtx_intern_symbol(ctx, FuStr_clone(s2));
    fu_sym_t sym3 = FuCtx_intern_symbol(ctx, FuStr_clone(s3));
    assert(sym1 == sym2);
    assert(sym1 != sym3);
    FuStr_drop(s1);
    FuStr_drop(s2);
    FuStr_drop(s3);
}

void test_keyword(FuCtx *ctx) {
    FuStr *kw_if = FuStr_from_utf8_cstr("if");
    fu_sym_t sym_if = FuCtx_intern_symbol(ctx, FuStr_clone(kw_if));
    assert(KW_IF == sym_if);
    FuStr_drop(kw_if);
}

/* must run after test_symbol */
void test_file(FuCtx *ctx) {
    FuStr *fpath = FuStr_from_utf8_cstr("hello");
    FuStr *content = FuStr_from_utf8_cstr("world");
    fu_sym_t sym = FuCtx_intern_symbol(ctx, FuStr_clone(fpath));
    FuCtx_intern_file(ctx, sym, content);
    assert(content == FuCtx_get_file(ctx, sym));
    FuStr_drop(fpath);
}

int main(void) {
    FuCtx *ctx = FuCtx_new(FuStr_new());
    FuCtx_init(ctx);
    test_symbol(ctx);
    test_keyword(ctx);
    test_file(ctx);
    FuCtx_drop(ctx);
    return 0;
}
