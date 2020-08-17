#include <assert.h>

#include "alloc.h"
#include "parse.h"

fu_sym_t FuKind_type_sym(FuCtx *ctx, fu_type_k kd) {
    char *cstr = FuKind_type_cstr(kd);
    FuStr *str = FuStr_from_utf8_cstr(cstr);
    fu_sym_t sym = FuCtx_intern_symbol(ctx, str);
    return sym;
}

FuType *FuType_new(FuCtx *ctx, fu_type_k kd, fu_vis_k vis) {
    FuType *ty = FuMem_new(FuType);
    ty->kd = kd;
    ty->vis = vis;
    ty->tid = FuCtx_push_type(ctx, ty);
    return ty;
}

void FuType_drop(FuType *ty) {
    FuMem_free(ty);
}

FuType *FuType_new_name(FuScope *scp, fu_sym_t name) {
    FuType *ty;
    ty = FuScope_get_type(scp, name);
    if (ty) {
        return ty;
    }
    /* todo: path type */
    ty = FuType_new(scp->ctx, TY_PATH, VIS_INHERIT);
    return ty;
}

void FuType_init_pkg_builtins(FuCtx *ctx, FuNode *nd) {
    assert(nd->kd == ND_PKG);

    fu_sym_t sym;
    fu_tid_t tid;

    /* create init tids */
    if (!FuVec_len(ctx->types)) {
        /* clang-format off */
#define TYPE(kd, name) \
        if(kd <= TY_ERR) { \
            FuType_new(ctx, kd, VIS_BUILTIN); \
        }
        #include "type.def"
        /* clang-format on */
#undef TYPE
    }

    /* insert builtins */
    /* clang-format off */
#define TYPE(kd, name) \
        if(kd <= TY_VA_LIST) { \
            tid = kd; \
            sym = FuKind_type_sym(ctx, kd); \
            FuMap_set(nd->_pkg.builtins->types, &sym, &tid, NULL); \
        }
        #include "type.def"
/* clang-format on */
#undef TYPE
}
