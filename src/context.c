#include "alloc.h"
#include "parse.h"

fu_bool_t FuSymbol_eq(FuSymbol *sym1, FuSymbol *sym2) {
    return *sym1 == *sym2;
}

fu_size_t FuSymbol_hash(FuSymbol *sym) {
    return hash_bytes((fu_uint8_t *)sym, sizeof(FuSymbol));
}

FuContext *FuContext_new() {
    FuContext *ctx = FuMem_new(FuContext);
    ctx->symbols = FuSet_with_capacity(1024 * 20, sizeof(FuStr *), (FuEqFn)FuStr_eq, (FuHashFn)FuStr_hash);
    ctx->fmap = FuMap_new(sizeof(FuSymbol), sizeof(fu_size_t), (FuEqFn)FuSymbol_eq, (FuHashFn)FuSymbol_hash);
    ctx->fcontents = FuVec_new(sizeof(FuStr *));
    return ctx;
}

void FuContext_init(FuContext *ctx) {
    /* must be inter keyword first
     * KW_keyword == SYM_keyword
     */
#define KEYWORD(kd, str) FuContext_intern_symbol(ctx, FuStr_from_utf8_cstr(str));
#include "keyword.def"
#undef KEYWORD
    /* init other symbol */
#define SYMBOL(str) FuContext_intern_symbol(ctx, FuStr_from_utf8_cstr(str));
#include "symbol.def"
#undef SYMBOL
}

void FuContext_drop(FuContext *ctx) {
    FuVec_drop_with_ptrs(ctx->fcontents, (FuDropFn)FuStr_drop);
    FuMap_drop(ctx->fmap);
    FuSet_drop_with_ptrs(ctx->symbols, (FuDropFn)FuStr_drop);
    FuMem_free(ctx);
}

FuSymbol FuContext_intern_symbol(FuContext *ctx, FuStr *symbol) {
    FuSymbol sym;
    if (FuSet_contains_ptr(ctx->symbols, symbol, &sym)) {
        FuStr_drop(symbol);
        return sym;
    }
    FuSet_add_ptr(ctx->symbols, symbol, &sym);
    return sym;
}

FuStr *FuContext_get_symbol(FuContext *ctx, FuSymbol sym) {
    return (FuStr *)FuSet_key_ptr_at(ctx->symbols, sym);
}

void FuContext_intern_file(FuContext *ctx, FuSymbol fpath, FuStr *fcontent) {
    if (FuMap_contains(ctx->fmap, &fpath)) {
        return;
    }
    fu_size_t idx = FuVec_len(ctx->fcontents);
    FuMap_set(ctx->fmap, &fpath, &idx, NULL);
    FuVec_push_ptr(ctx->fcontents, fcontent);
}

FuStr *FuContext_get_file(FuContext *ctx, FuSymbol fpath) {
    if (!FuMap_contains(ctx->fmap, &fpath)) {
        return NULL;
    }
    fu_size_t *idx_p = FuMap_get(ctx->fmap, &fpath);
    return FuVec_get_ptr(ctx->fcontents, *idx_p);
}
