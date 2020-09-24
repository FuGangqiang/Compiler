#include <assert.h>
#include <stdio.h>

#include "driver.h"
#include "parse.h"
#include "str.h"

void test_symbol(FuPkg *pkg) {
    FuStr *s1 = FuStr_from_utf8_cstr("hello");
    FuStr *s2 = FuStr_from_utf8_cstr("hello");
    FuStr *s3 = FuStr_from_utf8_cstr("world");
    fu_sym_t sym1 = FuPkg_intern_symbol(pkg, FuStr_clone(s1));
    fu_sym_t sym2 = FuPkg_intern_symbol(pkg, FuStr_clone(s2));
    fu_sym_t sym3 = FuPkg_intern_symbol(pkg, FuStr_clone(s3));
    assert(sym1 == sym2);
    assert(sym1 != sym3);
    FuStr_drop(s1);
    FuStr_drop(s2);
    FuStr_drop(s3);
}

void test_keyword(FuPkg *pkg) {
    FuStr *kw_if = FuStr_from_utf8_cstr("if");
    fu_sym_t sym_if = FuPkg_intern_symbol(pkg, FuStr_clone(kw_if));
    assert(KW_IF == sym_if);
    FuStr_drop(kw_if);
}

/* must run after test_symbol */
void test_file(FuPkg *pkg) {
    FuStr *fpath = FuStr_from_utf8_cstr("hello");
    FuStr *content = FuStr_from_utf8_cstr("world");
    fu_sym_t sym = FuPkg_intern_symbol(pkg, FuStr_clone(fpath));
    FuPkg_intern_file(pkg, sym, content);
    assert(content == FuPkg_get_file(pkg, sym));
    FuStr_drop(fpath);
}

int main(void) {
    FuConfig *cfg = FuConfig_new();
    cfg->input_fpath = FuStr_from_utf8_cstr("/tmp/test.fu");
    cfg->out_dir = FuStr_from_utf8_cstr("/tmp");
    FuConfig_init(cfg);
    FuPkg *pkg = FuPkg_new(cfg);
    FuPkg_init(pkg);
    test_symbol(pkg);
    test_keyword(pkg);
    test_file(pkg);
    FuPkg_drop(pkg);
    return 0;
}
