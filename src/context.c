#include "alloc.h"
#include "parse.h"

fu_bool_t FuId_eq(fu_id_t *id1, fu_id_t *id2) {
    return *id1 == *id2;
}

fu_size_t FuId_hash(fu_id_t *id) {
    return hash_bytes((fu_uint8_t *)id, sizeof(fu_id_t));
}

FuCtx *FuCtx_new() {
    FuCtx *ctx = FuMem_new(FuCtx);
    ctx->symbols = FuSet_with_capacity(1024 * 20, sizeof(FuStr *), (FuEqFn)FuStr_eq, (FuHashFn)FuStr_hash);
    ctx->fmap = FuMap_new(sizeof(fu_sym_t), sizeof(fu_size_t), (FuEqFn)FuId_eq, (FuHashFn)FuId_hash);
    ctx->fcontents = FuVec_new(sizeof(FuStr *));
    ctx->nodes = FuVec_new(sizeof(FuNode *));
    ctx->types = FuVec_new(sizeof(FuType *));
    return ctx;
}

void FuCtx_init(FuCtx *ctx) {
    /* must be inter keyword first
     * KW_keyword == SYM_keyword
     */
#define KEYWORD(kd, str) FuCtx_intern_symbol(ctx, FuStr_from_utf8_cstr(str));
#include "keyword.def"
#undef KEYWORD
    /* init other symbol */
#define SYMBOL(str) FuCtx_intern_symbol(ctx, FuStr_from_utf8_cstr(str));
#include "symbol.def"
#undef SYMBOL
}

void FuCtx_drop(FuCtx *ctx) {
    FuVec_drop_with_ptrs(ctx->types, (FuDropFn)FuType_drop);
    FuVec_drop_with_ptrs(ctx->nodes, (FuDropFn)FuNode_drop);
    FuVec_drop_with_ptrs(ctx->fcontents, (FuDropFn)FuStr_drop);
    FuMap_drop(ctx->fmap);
    FuSet_drop_with_ptrs(ctx->symbols, (FuDropFn)FuStr_drop);
    FuMem_free(ctx);
}

fu_sym_t FuCtx_intern_symbol(FuCtx *ctx, FuStr *symbol) {
    fu_sym_t sym;
    if (FuSet_contains_ptr(ctx->symbols, symbol, &sym)) {
        FuStr_drop(symbol);
        return sym;
    }
    FuSet_add_ptr(ctx->symbols, symbol, &sym);
    return sym;
}

FuStr *FuCtx_get_symbol(FuCtx *ctx, fu_sym_t sym) {
    return (FuStr *)FuSet_key_ptr_at(ctx->symbols, sym);
}

void FuCtx_intern_file(FuCtx *ctx, fu_sym_t fpath, FuStr *fcontent) {
    if (FuMap_contains(ctx->fmap, &fpath)) {
        return;
    }
    fu_size_t idx = FuVec_len(ctx->fcontents);
    FuMap_set(ctx->fmap, &fpath, &idx, NULL);
    FuVec_push_ptr(ctx->fcontents, fcontent);
}

FuStr *FuCtx_get_file(FuCtx *ctx, fu_sym_t fpath) {
    if (!FuMap_contains(ctx->fmap, &fpath)) {
        return NULL;
    }
    fu_size_t *idx_p = FuMap_get(ctx->fmap, &fpath);
    return FuVec_get_ptr(ctx->fcontents, *idx_p);
}

fu_tid_t FuCtx_push_type(FuCtx *ctx, FuType *ty) {
    fu_tid_t tid = FuVec_len(ctx->types);
    FuVec_push_ptr(ctx->types, ty);
    return tid;
}

FuType *FuCtx_get_type(FuCtx *ctx, fu_tid_t tid) {
    return FuVec_get_ptr(ctx->types, tid);
}
