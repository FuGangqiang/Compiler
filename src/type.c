#include <assert.h>

#include "alloc.h"
#include "log.h"
#include "parse.h"

fu_sym_t FuKind_type_to_sym(FuCtx *ctx, fu_type_k kd) {
    char *cstr = FuKind_type_cstr(kd);
    FuStr *str = FuStr_from_utf8_cstr(cstr);
    fu_sym_t sym = FuCtx_intern_symbol(ctx, str);
    return sym;
}

FuType *FuType_new(FuCtx *ctx, FuSpan *sp, fu_type_k kd) {
    FuType *ty = FuMem_new(FuType);
    ty->kd = kd;
    ty->sp = sp;
    ty->tid = FuCtx_push_type(ctx, ty);
    return ty;
}

FuType *FuType_from_keyword(FuCtx *ctx, FuSpan *sp, fu_keyword_k keyword) {
    FuType *ty;
    switch (keyword) {
    case KW_NIL:
        ty = FuType_new(ctx, sp, TY_NIL);
        break;
    case KW_UNDERSCORE:
        ty = FuType_new(ctx, sp, TY_AUTO);
        break;
    default:
        FATAL1(sp, "expect a type, find: `%s`", FuKind_keyword_cstr(keyword));
        break;
    }
    return ty;
}

FuType *FuType_new_path(FuCtx *ctx, FuPath *path) {
    FuType *ty = FuType_new(ctx, path->sp, TY_PATH);
    ty->_path.path = path;
    return ty;
}

FuType *FuType_new_fn_sig(FuCtx *ctx, FuSpan *sp, FuFnSig *sig) {
    FuType *ty = FuType_new(ctx, sp, TY_FN_SIG);
    ty->_fn_sig = sig;
    return ty;
}

void FuType_drop(FuType *ty) {
    if (!ty) {
        return;
    }
    switch (ty->kd) {
    case TY_PATH:
        FuPath_drop(ty->_path.path);
        break;
    case TY_ARRAY:
        FuExpr_drop(ty->_array.size);
        break;
    case TY_FN_SIG:
        FuFnSig_drop(ty->_fn_sig);
        break;
    default:
        break;
    }
    FuVec_drop(ty->attrs);
    FuMem_free(ty);
}

FuStr *FuType_display(FuType *ty) {
    FuStr *str = FuStr_new();
    switch (ty->kd) {
    case TY_NIL:
    case TY_NEVER:
    case TY_BOOL:
    case TY_BYTE:
    case TY_CHAR:
    case TY_I8:
    case TY_I16:
    case TY_I32:
    case TY_I64:
    case TY_INT:
    case TY_U8:
    case TY_U16:
    case TY_U32:
    case TY_U64:
    case TY_UINT:
    case TY_F32:
    case TY_F64:
    case TY_VA_LIST:
    case TY_AUTO:
    case TY_ERR:
        FuStr_push_utf8_cstr(str, FuKind_type_cstr(ty->kd));
        break;
    case TY_PATH:
        FuStr_append(str, FuPath_display(ty->_path.path));
        break;
    case TY_PTR: {
        fu_bool_t need_paren = FU_FALSE;
        FuStr_push_utf8_cstr(str, "*");
        if (FuType_precedence(ty) > FuType_precedence(ty->_ptr)) {
            FuStr_push_utf8_cstr(str, "(");
            need_paren = FU_TRUE;
        }
        FuStr_append(str, FuType_display(ty->_ptr));
        if (need_paren) {
            FuStr_push_utf8_cstr(str, ")");
        }
        break;
    }
    case TY_RAW_PTR: {
        fu_bool_t need_paren = FU_FALSE;
        FuStr_push_utf8_cstr(str, "*raw");
        if (FuType_precedence(ty) > FuType_precedence(ty->_ptr)) {
            FuStr_push_utf8_cstr(str, "(");
            need_paren = FU_TRUE;
        }
        FuStr_append(str, FuType_display(ty->_ptr));
        if (need_paren) {
            FuStr_push_utf8_cstr(str, ")");
        }
        break;
    }
    case TY_NILABLE: {
        fu_bool_t need_paren = FU_FALSE;
        if (FuType_precedence(ty) > FuType_precedence(ty->_ptr)) {
            FuStr_push_utf8_cstr(str, "(");
            need_paren = FU_TRUE;
        }
        FuStr_append(str, FuType_display(ty->_ptr));
        if (need_paren) {
            FuStr_push_utf8_cstr(str, ")");
        }
        FuStr_push_utf8_cstr(str, "?");
        break;
    }
    case TY_ARRAY:
        FuStr_push_utf8_cstr(str, "[");
        FuStr_append(str, FuType_display(ty->_array.ty));
        FuStr_push_utf8_format(str, "; %d", ty->_array.size->_lit->_int.v);
        FuStr_push_utf8_cstr(str, "]");
        break;
    case TY_SLICE:
        FuStr_push_utf8_cstr(str, "[");
        FuStr_append(str, FuType_display(ty->_slice));
        FuStr_push_utf8_cstr(str, "]");
        break;
    case TY_TUPLE: {
        FuStr_push_utf8_cstr(str, "(");
        fu_size_t len = FuVec_len(ty->_tuple);
        fu_size_t i;
        for (i = 0; i < len; i++) {
            FuType *item = FuVec_get_ptr(ty->_tuple, i);
            FuStr_append(str, FuType_display(item));
            if (i < len - 1) {
                FuStr_push_utf8_cstr(str, ", ");
            }
        }
        FuStr_push_utf8_cstr(str, ")");
        break;
    }
    case TY_FN_SIG:
        FuStr_append(str, FuFnSig_display(ty->_fn_sig));
        break;
    default:
        FATAL1(NULL, "unimplement type: `%s`", FuKind_type_cstr(ty->kd));
        break;
    }
    return str;
}

fu_op_prec_t FuType_precedence(FuType *ty) {
    switch (ty->kd) {
    case TY_NIL:
    case TY_NEVER:
    case TY_BOOL:
    case TY_BYTE:
    case TY_CHAR:
    case TY_I8:
    case TY_I16:
    case TY_I32:
    case TY_I64:
    case TY_INT:
    case TY_U8:
    case TY_U16:
    case TY_U32:
    case TY_U64:
    case TY_UINT:
    case TY_F32:
    case TY_F64:
    case TY_VA_LIST:
    case TY_AUTO:
    case TY_ERR:
    case TY_PATH:
    case TY_TUPLE:
        return FuTyOp_precedence(TY_OP_PTR) + 10;
        break;
    case TY_PTR:
    case TY_RAW_PTR:
        return FuTyOp_precedence(TY_OP_PTR);
        break;
    case TY_NILABLE:
        return FuTyOp_precedence(TY_OP_NILABLE);
        break;
    case TY_ARRAY:
    case TY_SLICE:
        return FuTyOp_precedence(TY_OP_ARRAY);
        break;
    case TY_FN_SIG:
        return FuTyOp_precedence(TY_OP_TRANS);
        break;
    default:
        break;
    }
    FATAL1(NULL, "unimplemented type: `%s`", FuKind_type_cstr(ty->kd));
    return 0;
}

void FuType_init_pkg_builtins(FuCtx *ctx, FuNode *nd) {
    assert(nd->kd == ND_PKG);

    fu_sym_t sym;
    fu_tid_t tid;
    FuType *ty;

    /* create init tids */
    if (!FuVec_len(ctx->types)) {
        /* clang-format off */
#define TYPE(kd, name)                      \
        if(kd <= TY_ERR) {                  \
            ty = FuType_new(ctx, NULL, kd); \
            ty->vis = VIS_BUILTIN;          \
        }
        #include "type.def"
        /* clang-format on */
#undef TYPE
    }

/* insert builtins */
/* clang-format off */
#define TYPE(kd, name)                                             \
        if(kd <= TY_VA_LIST) {                                     \
            tid = kd;                                              \
            sym = FuKind_type_to_sym(ctx, kd);                     \
            FuMap_set(nd->_pkg.builtins->types, &sym, &tid, NULL); \
        }
        #include "type.def"
/* clang-format on */
#undef TYPE
}
