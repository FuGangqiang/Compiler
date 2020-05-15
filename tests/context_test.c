#include <assert.h>
#include <stdio.h>

#include "parse.h"
#include "str.h"

void test_symbol(FuContext *ctx) {
    FuStr *s1 = FuStr_from_utf8_cstr("hello");
    FuStr *s2 = FuStr_from_utf8_cstr("hello");
    FuStr *s3 = FuStr_from_utf8_cstr("world");
    FuSymbol sym1 = FuContext_intern_symbol(ctx, FuStr_clone(s1));
    FuSymbol sym2 = FuContext_intern_symbol(ctx, FuStr_clone(s2));
    FuSymbol sym3 = FuContext_intern_symbol(ctx, FuStr_clone(s3));
    assert(sym1 == sym2);
    assert(sym1 != sym3);
    FuStr_drop(s1);
    FuStr_drop(s2);
    FuStr_drop(s3);
}

void test_keyword(FuContext *ctx) {
    FuStr *kw_if = FuStr_from_utf8_cstr("if");
    FuSymbol sym_if = FuContext_intern_symbol(ctx, FuStr_clone(kw_if));
    assert(KW_IF == sym_if);
    FuStr_drop(kw_if);
}

/* must run after test_symbol */
void test_file(FuContext *ctx) {
    FuStr *fpath = FuStr_from_utf8_cstr("hello");
    FuStr *content = FuStr_from_utf8_cstr("world");
    FuSymbol sym = FuContext_intern_symbol(ctx, FuStr_clone(fpath));
    FuContext_intern_file(ctx, sym, content);
    assert(content == FuContext_get_file(ctx, sym));
    FuStr_drop(fpath);
}

int main(void) {
    FuContext *ctx = FuContext_new();
    FuContext_init(ctx);
    test_symbol(ctx);
    test_keyword(ctx);
    test_file(ctx);
    FuContext_drop(ctx);
    return 0;
}
