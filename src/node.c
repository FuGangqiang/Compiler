#include "alloc.h"
#include "log.h"
#include "parse.h"

static void FuStr_push_indent(FuStr *str, fu_size_t n) {
    fu_size_t i;
    for (i = 0; i < n * DUMP_INDENT_WIDTH; i++) {
        FuStr_push(str, ' ');
    }
}

FuIdent *FuIdent_new(FuSpan *sp, fu_sym_t name) {
    FuIdent *ident = FuMem_new(FuIdent);
    ident->sp = sp;
    ident->name = name;
    return ident;
}

FuIdent *FuIdent_from_idx(FuCtx *ctx, FuSpan *sp, fu_size_t i) {
    FuStr *symbol = FuStr_new();
    FuStr_push_utf8_format(symbol, "%d", i);
    fu_sym_t sym = FuCtx_intern_symbol(ctx, symbol);
    FuIdent *ident = FuIdent_new(NULL, sym);
    ident->sp = sp;
    return ident;
}

void FuIdent_drop(FuIdent *ident) {
    if (!ident) {
        return;
    }
    FuMem_free(ident);
}

FuStr *FuIdent_display(FuIdent *ident) {
    FuStr *str = FuCtx_get_symbol(ident->sp->ctx, ident->name);
    return FuStr_clone(str);
}

FuLabel *FuLabel_new(FuSpan *sp, fu_sym_t name) {
    FuLabel *label = FuMem_new(FuLabel);
    label->sp = sp;
    label->name = name;
    return label;
}

void FuLabel_drop(FuLabel *label) {
    if (!label) {
        return;
    }
    FuMem_free(label);
}

FuStr *FuLabel_display(FuLabel *label) {
    FuStr *str = FuCtx_get_symbol(label->sp->ctx, label->name);
    return FuStr_clone(str);
}

FuTokTree *FuTokTree_new(FuSpan *sp, fu_bool_t is_group) {
    FuTokTree *tree = FuMem_new(FuTokTree);
    tree->sp = sp;
    tree->is_group = is_group;
    return tree;
}

void FuTokTree_drop(FuTokTree *tree) {
    if (!tree) {
        return;
    }
    if (tree->is_group) {
        FuVec_drop_with_ptrs(tree->_group.trees, (FuDropFn)FuTokTree_drop);
    }
    FuMem_free(tree);
}

FuStr *FuTokTree_display(FuTokTree *tree, fu_size_t indent) {
    FuStr *str = FuStr_new();
    FuStr_push_indent(str, indent);
    FuStr_push_utf8_format(str, "is_group: %d\n", tree->is_group);
    if (tree->is_group) {
        fu_size_t len = FuVec_len(tree->_group.trees);
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_format(str, "tok tree delimiter: ");
        FuStr_append(str, FuSpan_content(tree->_group.open_tok.sp));
        FuStr_push_utf8_cstr(str, "\n");
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_format(str, "group tree items len: %d\n", len);
        fu_size_t i;
        for (i = 0; i < len; i++) {
            FuTokTree *item = FuVec_get_ptr(tree->_group.trees, i);
            FuStr_append(str, FuTokTree_display(item, indent + 1));
        }
        return str;
    }
    FuStr_push_indent(str, indent);
    FuStr_push_utf8_cstr(str, "tree token: ");
    FuStr_append(str, FuSpan_content(tree->_token.sp));
    FuStr_push_utf8_cstr(str, "\n");
    return str;
}

FuAttr *FuAttr_new(FuSpan *sp, fu_attr_k kd, fu_bool_t is_outer) {
    FuAttr *attr = FuMem_new(FuAttr);
    attr->sp = sp;
    attr->kd = kd;
    attr->is_outer = is_outer;
    return attr;
}

void FuAttr_drop(FuAttr *attr) {
    if (!attr) {
        return;
    }
    switch (attr->kd) {
    case ATTR_NORMAL:
        FuPath_drop(attr->_normal.path);
        FuTokTree_drop(attr->_normal.tok_tree);
        break;
    case ATTR_DOC:
        FuStr_drop(attr->_doc);
        break;
    default:
        break;
    }
    FuMem_free(attr);
}

FuStr *FuAttr_display(FuAttr *attr, fu_size_t indent) {
    FuStr *str = FuStr_new();
    FuStr_push_indent(str, indent);
    FuStr_push_utf8_format(str, "kd: %s\n", FuKind_attr_cstr(attr->kd));
    FuStr_push_indent(str, indent);
    FuStr_push_utf8_format(str, "is_outer: %d\n", (attr->is_outer));
    switch (attr->kd) {
    case ATTR_NORMAL:
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "path: ");
        FuStr_append(str, FuPath_display(attr->_normal.path));
        FuStr_push_utf8_cstr(str, "\n");
        if (attr->_normal.tok_tree) {
            FuStr_push_indent(str, indent);
            FuStr_push_utf8_cstr(str, "tok trees:\n");
            FuStr_append(str, FuTokTree_display(attr->_normal.tok_tree, indent + 1));
        }
        break;
    case ATTR_DOC:
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "doc: ");
        FuStr_append(str, FuStr_clone(attr->_doc));
        FuStr_push_utf8_cstr(str, "\n");
        break;
    default:
        break;
    }
    return str;
}

void FuPathItem_drop(FuPathItem *item) {
    if (!item) {
        return;
    }
    /* todo generic
        item->ge_args
    */
    FuIdent_drop(item->ident);
    FuMem_free(item);
}

FuStr *FuPathItem_display(FuPathItem *item) {
    FuStr *str = FuStr_new();
    /* todo: generic */
    FuStr_append(str, FuIdent_display(item->ident));
    return str;
}

void FuPath_drop(FuPath *path) {
    if (!path) {
        return;
    }
    FuVec_drop_with_ptrs(path->segments, (FuDropFn)FuPathItem_drop);
    FuMem_free(path);
}

FuStr *FuPath_display(FuPath *path) {
    FuStr *str = FuStr_new();
    fu_size_t len = FuVec_len(path->segments);
    fu_size_t i;
    for (i = 0; i < len; i++) {
        FuPathItem *item = FuVec_get_ptr(path->segments, i);
        FuStr_append(str, FuPathItem_display(item));
        if (i < len - 1) {
            FuStr_push_utf8_cstr(str, "::");
        }
    }
    return str;
}

FuFieldDef *FuFieldDef_new(FuSpan *sp, FuVec *attrs, fu_vis_k vis) {
    FuFieldDef *def = FuMem_new(FuFieldDef);
    def->sp = sp;
    def->attrs = attrs;
    def->vis = vis;
    return def;
}

FuFieldDef *FuFieldDef_from_idx_type(FuVec *attrs, FuIdent *ident, FuType *ty) {
    FuFieldDef *def = FuMem_new(FuFieldDef);
    def->sp = ty->sp;
    def->attrs = attrs;
    def->vis = VIS_PUB;
    def->ident = ident;
    def->ty = ty;
    return def;
}

void FuFieldDef_drop(FuFieldDef *def) {
    if (!def) {
        return;
    }
    FuVec_drop_with_ptrs(def->attrs, (FuDropFn)FuAttr_drop);
    FuIdent_drop(def->ident);
    FuMem_free(def);
}

FuStr *FuFieldDef_display(FuFieldDef *def, fu_size_t indent) {
    FuStr *str = FuStr_new();
    fu_size_t attrs_len = def->attrs ? FuVec_len(def->attrs) : 0;
    if (attrs_len) {
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "attrs:\n");
        fu_size_t i;
        for (i = 0; i < attrs_len; i++) {
            FuAttr *item = FuVec_get_ptr(def->attrs, i);
            FuStr_append(str, FuAttr_display(item, indent + 1));
        }
    }
    FuStr_push_indent(str, indent);
    FuStr_push_utf8_format(str, "vis: %s\n", FuKind_vis_cstr(def->vis));
    FuStr_push_indent(str, indent);
    FuStr_push_utf8_cstr(str, "ident: ");
    FuStr_append(str, FuIdent_display(def->ident));
    FuStr_push_utf8_cstr(str, "\n");
    FuStr_push_indent(str, indent);
    FuStr_push_utf8_cstr(str, "ty: ");
    FuStr_append(str, FuType_display(def->ty));
    FuStr_push_utf8_cstr(str, "\n");
    return str;
}

FuFieldInit *FuFieldInit_new(FuSpan *sp, fu_field_k kd, FuVec *attrs) {
    FuFieldInit *init = FuMem_new(FuFieldInit);
    init->kd = kd;
    init->sp = sp;
    init->attrs = attrs;
    return init;
}

void FuFieldInit_drop(FuFieldInit *init) {
    if (!init) {
        return;
    }
    switch (init->kd) {
    case FLD_EXPR:
        FuExpr_drop(init->_expr);
        break;
    case FLD_NAME:
        FuExpr_drop(init->_name.init);
        FuIdent_drop(init->_name.ident);
        break;
    case FLD_REPEAT:
        FuExpr_drop(init->_repeat);
        break;
    case FLD_BASE:
        FuExpr_drop(init->_base);
        break;
    case FLD_INDEX:
        FuLit_drop(init->_index.lit);
        FuExpr_drop(init->_index.init);
        break;
    case FLD_SIZE:
        FuExpr_drop(init->_size);
        break;
    default:
        FATAL(NULL, "can not be here");
        break;
    }
    FuVec_drop_with_ptrs(init->attrs, (FuDropFn)FuAttr_drop);
    FuMem_free(init);
}

FuStr *FuFieldInit_display(FuFieldInit *init, fu_size_t indent) {
    FuStr *str = FuStr_new();
    FuStr_push_indent(str, indent);
    FuStr_push_utf8_format(str, "kd: %s\n", FuKind_field_cstr(init->kd));
    fu_size_t attrs_len = init->attrs ? FuVec_len(init->attrs) : 0;
    if (attrs_len) {
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "attrs:\n");
        fu_size_t i;
        for (i = 0; i < attrs_len; i++) {
            FuAttr *item = FuVec_get_ptr(init->attrs, i);
            FuStr_append(str, FuAttr_display(item, indent + 1));
        }
    }
    switch (init->kd) {
    case FLD_EXPR:
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "expr:\n");
        FuStr_append(str, FuExpr_display(init->_expr, indent + 1));
        break;
    case FLD_NAME:
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "name: ");
        FuStr_append(str, FuIdent_display(init->_name.ident));
        FuStr_push_utf8_cstr(str, "\n");
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "init:\n");
        FuStr_append(str, FuExpr_display(init->_name.init, indent + 1));
        break;
    case FLD_REPEAT:
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "repeat:\n");
        FuStr_append(str, FuExpr_display(init->_repeat, indent + 1));
        break;
    case FLD_BASE:
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "base:\n");
        FuStr_append(str, FuExpr_display(init->_base, indent + 1));
        break;
    case FLD_INDEX:
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "index:\n");
        FuStr_append(str, FuLit_display(init->_index.lit, indent + 1));
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "init:\n");
        FuStr_append(str, FuExpr_display(init->_index.init, indent + 1));
        break;
    case FLD_SIZE:
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "size:\n");
        FuStr_append(str, FuExpr_display(init->_size, indent + 1));
        break;
    default:
        FATAL(NULL, "can not be here");
        break;
    }
    return str;
}

FuVariant *FuVariant_new(FuSpan *sp, FuVec *attrs, fu_vis_k vis, fu_variant_k kd) {
    FuVariant *va = FuMem_new(FuVariant);
    va->sp = sp;
    va->attrs = attrs;
    va->vis = vis;
    va->kd = kd;
    return va;
}

void FuVariant_drop(FuVariant *va) {
    if (!va) {
        return;
    }
    FuVec_drop_with_ptrs(va->attrs, (FuDropFn)FuAttr_drop);
    FuIdent_drop(va->ident);
    switch (va->kd) {
    case VA_UNIT:
        FuLit_drop(va->_unit.init);
        break;
    case VA_STRUCT:
        FuVec_drop_with_ptrs(va->_struct.fields, (FuDropFn)FuFieldDef_drop);
        break;
    case VA_TUPLE:
        FuVec_drop_with_ptrs(va->_tuple.fields, (FuDropFn)FuFieldDef_drop);
        break;
    default:
        break;
    }
    FuMem_free(va);
}

FuStr *FuVariant_display(FuVariant *va, fu_size_t indent) {
    FuStr *str = FuStr_new();
    FuStr_push_indent(str, indent);
    FuStr_push_utf8_format(str, "kd: %s\n", FuKind_variant_cstr(va->kd));
    fu_size_t attrs_len = va->attrs ? FuVec_len(va->attrs) : 0;
    if (attrs_len) {
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "attrs:\n");
        fu_size_t i;
        for (i = 0; i < attrs_len; i++) {
            FuAttr *item = FuVec_get_ptr(va->attrs, i);
            FuStr_append(str, FuAttr_display(item, indent + 1));
        }
    }
    FuStr_push_indent(str, indent);
    FuStr_push_utf8_format(str, "vis: %s\n", FuKind_vis_cstr(va->vis));
    FuStr_push_indent(str, indent);
    FuStr_push_utf8_format(str, "ident: ");
    FuStr_append(str, FuIdent_display(va->ident));
    FuStr_push_utf8_format(str, "\n");
    switch (va->kd) {
    case VA_UNIT:
        if (va->_unit.init) {
            FuStr_push_indent(str, indent);
            FuStr_push_utf8_format(str, "init:\n");
            FuStr_append(str, FuLit_display(va->_unit.init, indent + 1));
        }
        break;
    case VA_STRUCT: {
        FuStr_push_indent(str, indent);
        fu_size_t len = FuVec_len(va->_struct.fields);
        FuStr_push_utf8_format(str, "struct len: %d\n", len);
        fu_size_t i;
        for (i = 0; i < len; i++) {
            FuFieldDef *item = FuVec_get_ptr(va->_struct.fields, i);
            FuStr_append(str, FuFieldDef_display(item, indent + 1));
        }
        break;
    }
    case VA_TUPLE: {
        FuStr_push_indent(str, indent);
        fu_size_t len = FuVec_len(va->_tuple.fields);
        FuStr_push_utf8_format(str, "tuple len: %d\n", len);
        fu_size_t i;
        for (i = 0; i < len; i++) {
            FuFieldDef *item = FuVec_get_ptr(va->_tuple.fields, i);
            FuStr_append(str, FuFieldDef_display(item, indent + 1));
        }
        break;
    }
    default:
        break;
    }
    return str;
}

FuFnParam *FuFnParam_new(FuSpan *sp, FuPat *pat) {
    FuFnParam *param = FuMem_new(FuFnParam);
    param->sp = sp;
    param->pat = pat;
    return param;
}

void FuFnParam_drop(FuFnParam *param) {
    if (!param) {
        return;
    }
    FuPat_drop(param->pat);
    FuVec_drop_with_ptrs(param->attrs, (FuDropFn)FuAttr_drop);
    FuMem_free(param);
}

FuStr *FuFnParam_display(FuFnParam *param, fu_size_t indent) {
    FuStr *str = FuStr_new();
    fu_size_t attrs_len = param->attrs ? FuVec_len(param->attrs) : 0;
    if (attrs_len) {
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "attrs:\n");
        fu_size_t i;
        for (i = 0; i < attrs_len; i++) {
            FuAttr *item = FuVec_get_ptr(param->attrs, i);
            FuStr_append(str, FuAttr_display(item, indent + 1));
        }
    }
    FuStr_push_indent(str, indent);
    FuStr_push_utf8_format(str, "pat:\n");
    FuStr_append(str, FuPat_display(param->pat, indent + 1));
    if (param->ty) {
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_format(str, "ty: ");
        FuStr_append(str, FuType_display(param->ty));
        FuStr_push_utf8_format(str, "\n");
    }
    return str;
}

FuFnSig *FuFnSig_new(FuGeneric *ge, FuVec *tys) {
    FuFnSig *sig = FuMem_new(FuFnSig);
    sig->ge = ge;
    sig->tys = tys;
    return sig;
}

FuFnSig *FuFnSig_from_params(FuCtx *ctx, FuGeneric *ge, FuVec *params, FuType *return_ty) {
    FuVec *sig_tys = FuVec_new(sizeof(FuType *));
    fu_size_t len = FuVec_len(params);
    fu_size_t i;
    for (i = 0; i < len; i++) {
        FuFnParam *param = FuVec_get_ptr(params, i);
        FuVec_push_ptr(sig_tys, param->ty);
    }
    if (len == 0) {
        FuVec_push_ptr(sig_tys, FuType_from_keyword(ctx, NULL, KW_NIL));
    }
    FuVec_push_ptr(sig_tys, return_ty);
    FuFnSig *sig = FuMem_new(FuFnSig);
    sig->ge = ge;
    sig->tys = sig_tys;
    return sig;
}

void FuFnSig_drop(FuFnSig *sig) {
    if (!sig) {
        return;
    }
    FuVec_drop(sig->tys);
    /* todo: generic */
    FuMem_free(sig);
}

FuStr *FuFnSig_display(FuFnSig *sig) {
    FuStr *str = FuStr_new();
    fu_size_t len = FuVec_len(sig->tys);
    fu_size_t i;
    for (i = 0; i < len; i++) {
        FuType *item = FuVec_get_ptr(sig->tys, i);
        fu_bool_t need_paren = FU_FALSE;
        if (FuTyOp_precedence(TY_OP_TRANS) >= FuType_precedence(item)) {
            FuStr_push_utf8_cstr(str, "(");
            need_paren = FU_TRUE;
        }
        FuStr_append(str, FuType_display(item));
        if (need_paren) {
            FuStr_push_utf8_cstr(str, ")");
        }
        if (i < len - 1) {
            FuStr_push_utf8_cstr(str, " -> ");
        }
    }
    return str;
}

FuArm *FuArm_new(FuSpan *sp, fu_arm_k kd) {
    FuArm *arm = FuMem_new(FuArm);
    arm->sp = sp;
    arm->kd = kd;
    return arm;
}

void FuArm_drop(FuArm *arm) {
    if (!arm) {
        return;
    }
    FuVec_drop_with_ptrs(arm->attrs, (FuDropFn)FuAttr_drop);
    FuPat_drop(arm->pat);
    FuExpr_drop(arm->guard);
    if (arm->kd == ARM_CATCH) {
        FuBlock_drop(arm->_catch.body);
    }
    FuMem_free(arm);
}

FuStr *FuArm_display(FuArm *arm, fu_size_t indent) {
    FuStr *str = FuStr_new();
    FuStr_push_indent(str, indent);
    FuStr_push_utf8_format(str, "kd: %s\n", FuKind_arm_cstr(arm->kd));
    fu_size_t attrs_len = arm->attrs ? FuVec_len(arm->attrs) : 0;
    if (attrs_len) {
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "attrs:\n");
        fu_size_t i;
        for (i = 0; i < attrs_len; i++) {
            FuAttr *item = FuVec_get_ptr(arm->attrs, i);
            FuStr_append(str, FuAttr_display(item, indent + 1));
        }
    }
    FuStr_push_indent(str, indent);
    FuStr_push_utf8_cstr(str, "pat:\n");
    FuStr_append(str, FuPat_display(arm->pat, indent + 1));
    if (arm->guard) {
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "guard:\n");
        FuStr_append(str, FuExpr_display(arm->guard, indent + 1));
    }
    FuStr_push_indent(str, indent);
    FuStr_push_utf8_cstr(str, "body:\n");
    switch (arm->kd) {
    case ARM_MATCH:
        FuStr_append(str, FuNode_display(arm->_match.body, indent + 1));
        break;
    case ARM_CATCH:
        FuStr_append(str, FuBlock_display(arm->_catch.body, indent + 1));
        break;
    default:
        FATAL(arm->sp, "can not be here");
        break;
    }
    return str;
}

FuAssoc *FuAssoc_new(FuSpan *sp, fu_assoc_k kd) {
    FuAssoc *assoc = FuMem_new(FuAssoc);
    assoc->sp = sp;
    assoc->kd = kd;
    return assoc;
}

void FuAssoc_drop(FuAssoc *assoc) {
    if (!assoc) {
        return;
    }
    FuIdent_drop(assoc->ident);
    switch (assoc->kd) {
    case ASSOC_CONST:
        FuLit_drop(assoc->_const.def);
        break;
    case ASSOC_TY_ALIAS:
        FuVec_drop(assoc->_ty_alias.bounds);
        break;
    case ASSOC_FN:
        FuVec_drop_with_ptrs(assoc->_fn.params, (FuDropFn)FuFnParam_drop);
        FuFnSig_drop(assoc->_fn.sig);
        FuBlock_drop(assoc->_fn.body);
        break;
    default:
        break;
    }
    FuVec_drop_with_ptrs(assoc->attrs, (FuDropFn)FuAttr_drop);
    FuMem_free(assoc);
}

FuStr *FuAssoc_display(FuAssoc *assoc, fu_size_t indent) {
    FuStr *str = FuStr_new();
    FuStr_push_indent(str, indent);
    FuStr_push_utf8_format(str, "kd: %s\n", FuKind_assoc_cstr(assoc->kd));
    fu_size_t attrs_len = assoc->attrs ? FuVec_len(assoc->attrs) : 0;
    if (attrs_len) {
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "attrs:\n");
        fu_size_t i;
        for (i = 0; i < attrs_len; i++) {
            FuAttr *item = FuVec_get_ptr(assoc->attrs, i);
            FuStr_append(str, FuAttr_display(item, indent + 1));
        }
    }
    FuStr_push_indent(str, indent);
    FuStr_push_utf8_format(str, "vis: %s\n", FuKind_vis_cstr(assoc->vis));
    FuStr_push_indent(str, indent);
    FuStr_push_utf8_cstr(str, "ident: ");
    FuStr_append(str, FuIdent_display(assoc->ident));
    FuStr_push_utf8_cstr(str, "\n");
    switch (assoc->kd) {
    case ASSOC_CONST:
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "ty: ");
        FuStr_append(str, FuType_display(assoc->_const.ty));
        FuStr_push_utf8_cstr(str, "\n");
        if (assoc->_const.def) {
            FuStr_push_indent(str, indent);
            FuStr_push_utf8_format(str, "default:\n");
            FuStr_append(str, FuLit_display(assoc->_const.def, indent + 1));
        }
        break;
    case ASSOC_TY_ALIAS: {
        fu_size_t len = FuVec_len(assoc->_ty_alias.bounds);
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_format(str, "bounds len: %d\n", len);
        if (len) {
            FuStr_push_indent(str, indent);
            FuStr_push_utf8_cstr(str, "items:\n");
            fu_size_t i;
            for (i = 0; i < len; i++) {
                FuStr_push_indent(str, indent + 1);
                FuType *item = FuVec_get_ptr(assoc->_ty_alias.bounds, i);
                FuStr_append(str, FuType_display(item));
                FuStr_push_utf8_cstr(str, "\n");
            }
        }
        if (assoc->_ty_alias.ty) {
            FuStr_push_indent(str, indent);
            FuStr_push_utf8_cstr(str, "ty: ");
            FuStr_append(str, FuType_display(assoc->_ty_alias.ty));
            FuStr_push_utf8_cstr(str, "\n");
        }
        break;
    }
    case ASSOC_FN: {
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "sig: ");
        FuStr_append(str, FuFnSig_display(assoc->_fn.sig));
        FuStr_push_utf8_cstr(str, "\n");
        fu_size_t len = FuVec_len(assoc->_fn.params);
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_format(str, "params len: %d\n", len);
        if (len > 0) {
            fu_size_t i;
            for (i = 0; i < len; i++) {
                FuFnParam *item = FuVec_get_ptr(assoc->_fn.params, i);
                FuStr_append(str, FuFnParam_display(item, indent + 1));
            }
        }
        if (assoc->_fn.body) {
            FuStr_push_indent(str, indent);
            FuStr_push_utf8_cstr(str, "body:\n");
            FuStr_append(str, FuBlock_display(assoc->_fn.body, indent + 1));
        }
        break;
    }
    default:
        break;
    }
    return str;
}

FuLit *FuLit_new(FuSpan *sp, fu_lit_k kind) {
    FuLit *lit = FuMem_new(FuLit);
    lit->sp = sp;
    lit->kd = kind;
    return lit;
}

void FuLit_drop(FuLit *lit) {
    if (!lit) {
        return;
    }
    switch (lit->kd) {
    case LIT_ERR:
    case LIT_NIL:
    case LIT_BOOL:
    case LIT_BYTE:
    case LIT_CHAR:
    case LIT_INT:
    case LIT_FLOAT:
        break;
    case LIT_STR:
        FuStr_drop(lit->_str);
        break;
    case LIT_BYTE_STR:
        FuBytes_drop(lit->_byte_str);
        break;
    case LIT_FORMAT_STR:
        FuStr_drop(lit->_format_str);
        break;
    default:
        FATAL(NULL, "can not be here!");
    }
    FuMem_free(lit);
}

FuStr *FuLit_display(FuLit *lit, fu_size_t indent) {
    FuStr *str = FuStr_new();
    FuStr_push_indent(str, indent);
    FuStr_push_utf8_format(str, "kd: %s\n", FuKind_lit_cstr(lit->kd));
    if (lit->kd != LIT_ERR) {
        FuStr_push_indent(str, indent);
    }
    switch (lit->kd) {
    case LIT_ERR:
        break;
    case LIT_NIL:
        FuStr_push_utf8_cstr(str, "v: nil\n");
        break;
    case LIT_BOOL:
        FuStr_push_utf8_format(str, "v: %d\n", lit->_bool.v);
        break;
    case LIT_BYTE:
        FuStr_push_utf8_format(str, "v: %d\n", lit->_byte.v);
        break;
    case LIT_CHAR:
        FuStr_push_utf8_format(str, "v: %d\n", lit->_char.v);
        break;
    case LIT_INT:
        FuStr_push_utf8_format(str, "size: %d, is_signed: %d, v: %llu\n", lit->_int.size, lit->_int.is_signed,
                               lit->_int.v);
        break;
    case LIT_STR:
        FuStr_push_utf8_format(str, "len: %d, ", FuStr_len(lit->_str));
        FuStr_push_utf8_cstr(str, "v: ");
        FuStr_append(str, FuStr_clone(lit->_str));
        FuStr_push_utf8_cstr(str, "\n");
        break;
    case LIT_BYTE_STR:
        FuStr_push_utf8_format(str, "len: %d, ", FuBytes_len(lit->_byte_str));
        FuStr_push_utf8_cstr(str, "v: ");
        FuStr_append(str, FuBytes_to_str(FuBytes_clone(lit->_byte_str)));
        FuStr_push_utf8_cstr(str, "\n");
        break;
    default:
        FATAL1(lit->sp, "unimplemented lit: `%s`\n", FuKind_lit_cstr(lit->kd));
        break;
    }
    return str;
}

FuPat *FuPat_new(FuSpan *sp, fu_pat_k kd) {
    FuPat *pat = FuMem_new(FuPat);
    pat->kd = kd;
    pat->sp = sp;
    return pat;
}

FuPat *FuPat_new_path_from_tok(FuToken tok) {
    FuIdent *ident = FuIdent_new(tok.sp, tok.sym);

    FuPathItem *path_item = FuMem_new(FuPathItem);
    path_item->sp = tok.sp;
    path_item->ident = ident;
    FuVec *items = FuVec_new(sizeof(FuPathItem *));
    FuVec_push_ptr(items, path_item);

    FuPath *path = FuMem_new(FuPath);
    path->sp = tok.sp;
    path->segments = items;
    FuExpr *expr = FuExpr_new_path(NULL, path);

    FuPat *pat = FuPat_new(tok.sp, PAT_EXPR);
    pat->_expr = expr;
    return pat;
}

void FuPat_drop(FuPat *pat) {
    if (!pat) {
        return;
    }
    switch (pat->kd) {
    case PAT_WILD:
        break;
    case PAT_EXPR:
        FuExpr_drop(pat->_expr);
        break;
    case PAT_INDEX:
        FuLit_drop(pat->_index);
        break;
    case PAT_FIELD:
        FuIdent_drop(pat->_field);
        break;
    case PAT_REPEAT:
        FuPat_drop(pat->_repeat);
        break;
    case PAT_BASE:
        FuExpr_drop(pat->_base);
        break;
    case PAT_BIND:
        FuIdent_drop(pat->_bind.ident);
        FuPat_drop(pat->_bind.pat);
        break;
    case PAT_OR:
        FuVec_drop_with_ptrs(pat->_or.pats, (FuDropFn)FuPat_drop);
        break;
    case PAT_SLICE:
        FuVec_drop_with_ptrs(pat->_slice.pats, (FuDropFn)FuPat_drop);
        break;
    case PAT_TUPLE:
        FuVec_drop_with_ptrs(pat->_tuple.pats, (FuDropFn)FuPat_drop);
        break;
    case PAT_STRUCT:
        FuExpr_drop(pat->_struct.path);
        FuVec_drop_with_ptrs(pat->_struct.pats, (FuDropFn)FuPat_drop);
        break;
    case PAT_TUPLE_STRUCT:
        FuExpr_drop(pat->_tuple_struct.path);
        FuVec_drop_with_ptrs(pat->_tuple_struct.pats, (FuDropFn)FuPat_drop);
        break;
    default:
        FATAL1(pat->sp, "unimplement pat: `%s`", FuKind_pat_cstr(pat->kd));
        break;
    }
    FuMem_free(pat);
}

FuStr *FuPat_display(FuPat *pat, fu_size_t indent) {
    FuStr *str = FuStr_new();
    FuStr_push_indent(str, indent);
    FuStr_push_utf8_format(str, "kd: %s\n", FuKind_pat_cstr(pat->kd));
    switch (pat->kd) {
    case PAT_WILD:
        break;
    case PAT_EXPR:
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "expr:\n");
        FuStr_append(str, FuExpr_display(pat->_expr, indent + 1));
        break;
    case PAT_INDEX:
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "indent:\n");
        FuStr_append(str, FuLit_display(pat->_index, indent + 1));
        break;
    case PAT_FIELD:
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "field: ");
        FuStr_append(str, FuIdent_display(pat->_field));
        FuStr_push_utf8_cstr(str, "\n");
        break;
    case PAT_REPEAT:
        if (pat->_repeat) {
            FuStr_push_indent(str, indent);
            FuStr_push_utf8_cstr(str, "repeat:\n");
            FuStr_append(str, FuPat_display(pat->_repeat, indent + 1));
        }
        break;
    case PAT_BASE:
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "base:\n");
        FuStr_append(str, FuExpr_display(pat->_base, indent + 1));
        break;
    case PAT_BIND:
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_format(str, "is_ref: %d\n", pat->_bind.is_ref);
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "ident: ");
        FuStr_append(str, FuIdent_display(pat->_bind.ident));
        FuStr_push_utf8_cstr(str, "\n");
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "pat:\n");
        FuStr_append(str, FuPat_display(pat->_bind.pat, indent + 1));
        break;
    case PAT_OR: {
        fu_size_t len = FuVec_len(pat->_or.pats);
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_format(str, "len: %d\n", len);
        if (len == 0) {
            break;
        }
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "pats:\n");
        fu_size_t i;
        for (i = 0; i < len; i++) {
            FuPat *item = FuVec_get_ptr(pat->_or.pats, i);
            FuStr_append(str, FuPat_display(item, indent + 1));
        }
        break;
    }
    case PAT_SLICE: {
        fu_size_t len = FuVec_len(pat->_slice.pats);
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_format(str, "len: %d\n", len);
        if (len == 0) {
            break;
        }
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "pats:\n");
        fu_size_t i;
        for (i = 0; i < len; i++) {
            FuPat *item = FuVec_get_ptr(pat->_slice.pats, i);
            FuStr_append(str, FuPat_display(item, indent + 1));
        }
        break;
    }
    case PAT_TUPLE: {
        fu_size_t len = FuVec_len(pat->_tuple.pats);
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_format(str, "len: %d\n", len);
        if (len == 0) {
            break;
        }
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "pats:\n");
        fu_size_t i;
        for (i = 0; i < len; i++) {
            FuPat *item = FuVec_get_ptr(pat->_tuple.pats, i);
            FuStr_append(str, FuPat_display(item, indent + 1));
        }
        break;
    }
    case PAT_STRUCT: {
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "path:\n");
        FuStr_append(str, FuExpr_display(pat->_struct.path, indent + 1));
        fu_size_t len = FuVec_len(pat->_struct.pats);
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_format(str, "len: %d\n", len);
        if (len == 0) {
            break;
        }
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "pats:\n");
        fu_size_t i;
        for (i = 0; i < len; i++) {
            FuPat *item = FuVec_get_ptr(pat->_struct.pats, i);
            FuStr_append(str, FuPat_display(item, indent + 1));
        }
        break;
    }
    case PAT_TUPLE_STRUCT: {
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "path:\n");
        FuStr_append(str, FuExpr_display(pat->_tuple_struct.path, indent + 1));
        fu_size_t len = FuVec_len(pat->_tuple_struct.pats);
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_format(str, "len: %d\n", len);
        if (len == 0) {
            break;
        }
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "pats:\n");
        fu_size_t i;
        for (i = 0; i < len; i++) {
            FuPat *item = FuVec_get_ptr(pat->_tuple_struct.pats, i);
            FuStr_append(str, FuPat_display(item, indent + 1));
        }
        break;
    }
    default:
        FATAL1(pat->sp, "unimplement pat: `%s`", FuKind_pat_cstr(pat->kd));
        break;
    }
    return str;
}

FuBlock *FuBlock_new(FuSpan *sp) {
    FuBlock *blk = FuMem_new(FuBlock);
    blk->sp = sp;
    return blk;
}

void FuBlock_drop(FuBlock *blk) {
    if (!blk) {
        return;
    }
    FuScope_drop(blk->scope);
    FuVec_drop(blk->items);
    FuMem_free(blk);
}

FuStr *FuBlock_display(FuBlock *blk, fu_size_t indent) {
    FuStr *str = FuStr_new();
    fu_size_t len = FuVec_len(blk->items);
    if (len == 0) {
        return str;
    }
    FuStr_push_indent(str, indent);
    FuStr_push_utf8_cstr(str, "items:\n");
    fu_size_t i;
    for (i = 0; i < len; i++) {
        FuNode *item = FuVec_get_ptr(blk->items, i);
        FuStr_append(str, FuNode_display(item, indent + 1));
    }
    return str;
}

FuUse *FuUse_new(FuSpan *sp, fu_use_k kd, FuPath *prefix) {
    FuUse *use = FuMem_new(FuUse);
    use->sp = sp;
    use->kd = kd;
    use->prefix = prefix;
    return use;
}

void FuUse_drop(FuUse *use) {
    if (!use) {
        return;
    }
    FuPath_drop(use->prefix);
    switch (use->kd) {
    case USE_SIMPLE:
        FuIdent_drop(use->_simple.alias);
        break;
    case USE_MACRO:
        FuIdent_drop(use->_macro.name);
        FuIdent_drop(use->_macro.alias);
        break;
    case USE_NESTED:
        FuVec_drop_with_ptrs(use->_nested, (FuDropFn)FuUse_drop);
        break;
    default:
        break;
    }
    FuMem_free(use);
}

FuStr *FuUse_display(FuUse *use, fu_size_t indent) {
    FuStr *str = FuStr_new();
    FuStr_push_indent(str, indent);
    FuStr_push_utf8_format(str, "kd: %s\n", FuKind_use_cstr(use->kd));
    if (use->prefix) {
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "prefix: ");
        FuStr_append(str, FuPath_display(use->prefix));
        FuStr_push_utf8_cstr(str, "\n");
    }
    switch (use->kd) {
    case USE_SIMPLE:
        if (use->_simple.alias) {
            FuStr_push_indent(str, indent);
            FuStr_push_utf8_cstr(str, "alias: ");
            FuStr_append(str, FuIdent_display(use->_simple.alias));
            FuStr_push_utf8_cstr(str, "\n");
        }
        break;
    case USE_MACRO:
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "macro: ");
        FuStr_append(str, FuIdent_display(use->_macro.name));
        FuStr_push_utf8_cstr(str, "\n");
        if (use->_macro.alias) {
            FuStr_push_indent(str, indent);
            FuStr_push_utf8_cstr(str, "alias: ");
            FuStr_append(str, FuIdent_display(use->_macro.alias));
            FuStr_push_utf8_cstr(str, "\n");
        }
        break;
    case USE_NESTED: {
        fu_size_t len = FuVec_len(use->_nested);
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_format(str, "nested len: %d\n", len);
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "nested:\n");
        fu_size_t i;
        for (i = 0; i < len; i++) {
            FuUse *item = FuVec_get_ptr(use->_nested, i);
            FuStr_append(str, FuUse_display(item, indent + 1));
        }
        break;
    }
    default:
        break;
    }
    return str;
}

FuExpr *FuExpr_new(FuSpan *sp, fu_expr_k kd) {
    FuExpr *expr = FuMem_new(FuExpr);
    expr->sp = sp;
    expr->kd = kd;
    return expr;
}

FuExpr *FuExpr_new_lit(FuLit *lit) {
    FuExpr *expr = FuExpr_new(lit->sp, EXPR_LIT);
    expr->_lit = lit;
    return expr;
}

FuExpr *FuExpr_new_path(FuAnnoSelf *anno, FuPath *path) {
    FuSpan *sp;
    if (anno) {
        sp = FuSpan_join(anno->sp, path->sp);
    } else {
        sp = path->sp;
    }
    FuExpr *expr = FuExpr_new(sp, EXPR_PATH);
    expr->_path.anno = anno;
    expr->_path.path = path;
    return expr;
}

fu_bool_t FuExpr_can_endwith_semi(FuExpr *expr) {
    switch (expr->kd) {
    case EXPR_CALL:
    case EXPR_METHOD_CALL:
        return FU_TRUE;
    default:
        return FU_FALSE;
        break;
    }
}

void FuExpr_drop(FuExpr *expr) {
    if (!expr) {
        return;
    }
    switch (expr->kd) {
    case EXPR_LIT:
        FuLit_drop(expr->_lit);
        break;
    case EXPR_PATH:
        /* todo: expr->_path.anno */
        FuPath_drop(expr->_path.path);
        break;
    case EXPR_ARRAY:
        FuVec_drop_with_ptrs(expr->_array.field_inits, (FuDropFn)FuFieldInit_drop);
        break;
    case EXPR_TUPLE:
        FuVec_drop_with_ptrs(expr->_tuple.fields, (FuDropFn)FuExpr_drop);
        break;
    case EXPR_STRUCT:
        FuExpr_drop(expr->_struct.base);
        FuVec_drop_with_ptrs(expr->_struct.field_inits, (FuDropFn)FuFieldInit_drop);
        break;
    case EXPR_TUPLE_STRUCT:
        FuExpr_drop(expr->_tuple_struct.base);
        FuVec_drop_with_ptrs(expr->_tuple_struct.field_inits, (FuDropFn)FuFieldInit_drop);
        break;
    case EXPR_RANGE:
        FuExpr_drop(expr->_range.start);
        FuExpr_drop(expr->_range.end);
        break;
    case EXPR_FIELD:
        FuIdent_drop(expr->_field.ident);
        FuExpr_drop(expr->_field.base);
        break;
    case EXPR_INDEX:
        FuExpr_drop(expr->_index.base);
        FuExpr_drop(expr->_index.idx);
        break;
    case EXPR_CAST:
        FuExpr_drop(expr->_cast.expr);
        break;
    case EXPR_CALL:
        FuExpr_drop(expr->_call.base);
        FuVec_drop_with_ptrs(expr->_call.args, (FuDropFn)FuExpr_drop);
        break;
    case EXPR_METHOD_CALL:
        FuPathItem_drop(expr->_method_call.method);
        FuVec_drop_with_ptrs(expr->_method_call.args, (FuDropFn)FuExpr_drop);
        break;
    case EXPR_UNARY:
        FuExpr_drop(expr->_unary.expr);
        break;
    case EXPR_BINARY:
        FuExpr_drop(expr->_binary.lexpr);
        FuExpr_drop(expr->_binary.rexpr);
        break;
    case EXPR_AWAIT:
        FuExpr_drop(expr->_await.expr);
        break;
    case EXPR_CLOSURE:
        FuVec_drop_with_ptrs(expr->_closure.params, (FuDropFn)FuFnParam_drop);
        FuExpr_drop(expr->_closure.body);
        break;
    case EXPR_LET_COND:
        FuPat_drop(expr->_let_cond.pat);
        FuExpr_drop(expr->_let_cond.expr);
        break;
    case EXPR_IF:
        FuExpr_drop(expr->_if.cond);
        FuExpr_drop(expr->_if.on_true);
        FuExpr_drop(expr->_if.on_false);
        break;
    default:
        FATAL1(expr->sp, "unimplemented expr: `%s`", FuKind_expr_cstr(expr->kd));
    }
    FuMem_free(expr);
}

FuStr *FuExpr_display(FuExpr *expr, fu_size_t indent) {
    FuStr *str = FuStr_new();
    if (!expr) {
        return str;
    }

    FuStr_push_indent(str, indent);
    FuStr_push_utf8_format(str, "kd: %s\n", FuKind_expr_cstr(expr->kd));
    switch (expr->kd) {
    case EXPR_LIT:
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "lit:\n");
        FuStr_append(str, FuLit_display(expr->_lit, indent + 1));
        break;
    case EXPR_PATH:
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "path: ");
        /* todo: expr->_path.anno */
        FuStr_append(str, FuPath_display(expr->_path.path));
        FuStr_push(str, '\n');
        break;
    case EXPR_ARRAY: {
        fu_size_t len = FuVec_len(expr->_array.field_inits);
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_format(str, "field inits len: %d\n", len);
        if (len > 0) {
            FuStr_push_indent(str, indent);
            FuStr_push_utf8_cstr(str, "field inits:\n");
            fu_size_t i;
            for (i = 0; i < len; i++) {
                FuFieldInit *item = FuVec_get_ptr(expr->_array.field_inits, i);
                FuStr_append(str, FuFieldInit_display(item, indent + 1));
            }
        }
        break;
    }
    case EXPR_TUPLE: {
        FuStr_push_indent(str, indent);
        fu_size_t len = FuVec_len(expr->_tuple.fields);
        FuStr_push_utf8_format(str, "len: %d\n", len);
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "items:\n");
        fu_size_t i;
        for (i = 0; i < len; i++) {
            FuExpr *item = FuVec_get_ptr(expr->_tuple.fields, i);
            FuStr_append(str, FuExpr_display(item, indent + 1));
        }
        break;
    }
    case EXPR_STRUCT: {
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "base:\n");
        FuStr_append(str, FuExpr_display(expr->_struct.base, indent + 1));
        fu_size_t len = FuVec_len(expr->_struct.field_inits);
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_format(str, "field inits len: %d\n", len);
        if (len > 0) {
            FuStr_push_indent(str, indent);
            FuStr_push_utf8_cstr(str, "field inits:\n");
            fu_size_t i;
            for (i = 0; i < len; i++) {
                FuFieldInit *item = FuVec_get_ptr(expr->_struct.field_inits, i);
                FuStr_append(str, FuFieldInit_display(item, indent + 1));
            }
        }
        break;
    }
    case EXPR_TUPLE_STRUCT: {
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "base:\n");
        FuStr_append(str, FuExpr_display(expr->_tuple_struct.base, indent + 1));
        fu_size_t len = FuVec_len(expr->_tuple_struct.field_inits);
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_format(str, "field inits len: %d\n", len);
        if (len > 0) {
            FuStr_push_indent(str, indent);
            FuStr_push_utf8_cstr(str, "field inits:\n");
            fu_size_t i;
            for (i = 0; i < len; i++) {
                FuFieldInit *item = FuVec_get_ptr(expr->_tuple_struct.field_inits, i);
                FuStr_append(str, FuFieldInit_display(item, indent + 1));
            }
        }
        break;
    }
    case EXPR_RANGE:
        if (expr->_range.start) {
            FuStr_push_indent(str, indent);
            FuStr_push_utf8_cstr(str, "start:\n");
            FuStr_append(str, FuExpr_display(expr->_range.start, indent + 1));
        }
        if (expr->_range.end) {
            FuStr_push_indent(str, indent);
            FuStr_push_utf8_cstr(str, "end:\n");
            FuStr_append(str, FuExpr_display(expr->_range.end, indent + 1));
        }
        break;
    case EXPR_FIELD:
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "base:\n");
        FuStr_append(str, FuExpr_display(expr->_field.base, indent + 1));
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "ident: ");
        FuStr_append(str, FuIdent_display(expr->_field.ident));
        FuStr_push_utf8_cstr(str, "\n");
        break;
    case EXPR_INDEX:
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "obj:\n");
        FuStr_append(str, FuExpr_display(expr->_index.base, indent + 1));
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "idx:\n");
        FuStr_append(str, FuExpr_display(expr->_index.idx, indent + 1));
        break;
    case EXPR_CAST:
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "ty: ");
        FuStr_append(str, FuType_display(expr->_cast.ty));
        FuStr_push_utf8_cstr(str, "\n");
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "expr:\n");
        FuStr_append(str, FuExpr_display(expr->_cast.expr, indent + 1));
        break;
    case EXPR_CALL: {
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "fn:\n");
        FuStr_append(str, FuExpr_display(expr->_call.base, indent + 1));
        fu_size_t len = FuVec_len(expr->_call.args);
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_format(str, "args len: %d\n", len);
        if (len > 0) {
            FuStr_push_indent(str, indent);
            FuStr_push_utf8_cstr(str, "args:\n");
            fu_size_t i;
            for (i = 0; i < len; i++) {
                FuExpr *item = FuVec_get_ptr(expr->_call.args, i);
                FuStr_append(str, FuExpr_display(item, indent + 1));
            }
        }
        break;
    }
    case EXPR_METHOD_CALL: {
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "method: ");
        FuStr_append(str, FuPathItem_display(expr->_method_call.method));
        FuStr_push_utf8_cstr(str, "\n");
        fu_size_t len = FuVec_len(expr->_method_call.args);
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_format(str, "args len: %d\n", len);
        if (len > 0) {
            FuStr_push_indent(str, indent);
            FuStr_push_utf8_cstr(str, "args:\n");
            fu_size_t i;
            for (i = 0; i < len; i++) {
                FuExpr *item = FuVec_get_ptr(expr->_method_call.args, i);
                FuStr_append(str, FuExpr_display(item, indent + 1));
            }
        }
        break;
    }
    case EXPR_UNARY:
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_format(str, "op: %s\n", FuKind_op_cstr(expr->_binary.op));
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "expr:\n");
        FuStr_append(str, FuExpr_display(expr->_unary.expr, indent + 1));
        break;
    case EXPR_BINARY:
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_format(str, "op: %s\n", FuKind_op_cstr(expr->_binary.op));
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "lexpr:\n");
        FuStr_append(str, FuExpr_display(expr->_binary.lexpr, indent + 1));
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "rexpr:\n");
        FuStr_append(str, FuExpr_display(expr->_binary.rexpr, indent + 1));
        break;
    case EXPR_AWAIT:
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "expr:\n");
        FuStr_append(str, FuExpr_display(expr->_await.expr, indent + 1));
        break;
    case EXPR_CLOSURE:
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_format(str, "is_async: %d\n", expr->_closure.is_async);
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_format(str, "is_unsafe: %d\n", expr->_closure.is_unsafe);
        if (expr->_closure.params) {
            FuStr_push_indent(str, indent);
            FuStr_push_utf8_cstr(str, "params:\n");
            fu_size_t len = FuVec_len(expr->_closure.params);
            fu_size_t i;
            for (i = 0; i < len; i++) {
                FuFnParam *param = FuVec_get_ptr(expr->_closure.params, i);
                FuStr_append(str, FuFnParam_display(param, indent + 1));
            }
        }
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "body:\n");
        FuStr_append(str, FuExpr_display(expr->_closure.body, indent + 1));
        break;
    case EXPR_LET_COND:
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "pat:\n");
        FuStr_append(str, FuPat_display(expr->_let_cond.pat, indent + 1));
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "expr:\n");
        FuStr_append(str, FuExpr_display(expr->_let_cond.expr, indent + 1));
        break;
    case EXPR_IF:
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "cond:\n");
        FuStr_append(str, FuExpr_display(expr->_if.cond, indent + 1));
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "on_true:\n");
        FuStr_append(str, FuExpr_display(expr->_if.on_true, indent + 1));
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "on_false:\n");
        FuStr_append(str, FuExpr_display(expr->_if.on_false, indent + 1));
        break;
    default:
        FATAL1(expr->sp, "unimplemented expr: `%s`", FuKind_expr_cstr(expr->kd));
    }
    return str;
}

FuNode *FuNode_new(FuCtx *ctx, FuSpan *sp, fu_node_k kind) {
    FuNode *node = FuMem_new(FuNode);
    node->sp = sp;
    node->kd = kind;
    node->nid = FuVec_len(ctx->nodes);
    FuVec_push_ptr(ctx->nodes, node);
    return node;
}

void FuNode_drop(FuNode *nd) {
    if (!nd) {
        return;
    }
    switch (nd->kd) {
    case ND_EXPR:
        FuExpr_drop(nd->_expr.expr);
        break;
    case ND_USE:
        FuUse_drop(nd->_use.tree);
        break;
    case ND_STATIC:
        FuExpr_drop(nd->_static.init);
        FuIdent_drop(nd->_static.ident);
        break;
    case ND_CONST:
        FuExpr_drop(nd->_const.init);
        FuIdent_drop(nd->_const.ident);
        break;
    case ND_LET:
        FuPat_drop(nd->_let.pat);
        FuExpr_drop(nd->_let.init);
        break;
    case ND_ASSIGN:
        FuExpr_drop(nd->_assign.lexpr);
        FuExpr_drop(nd->_assign.rexpr);
        break;
    case ND_BREAK:
        FuLabel_drop(nd->_break.label);
        break;
    case ND_CONTINUE:
        FuLabel_drop(nd->_continue.label);
        break;
    case ND_YIELD:
        FuExpr_drop(nd->_yield.expr);
        break;
    case ND_THROW:
        FuExpr_drop(nd->_throw.expr);
        break;
    case ND_RETURN:
        FuExpr_drop(nd->_return.expr);
        break;
    case ND_BLOCK:
        FuBlock_drop(nd->_block.block);
        break;
    case ND_IF:
        FuExpr_drop(nd->_if.cond);
        FuBlock_drop(nd->_if.block);
        break;
    case ND_MATCH:
        FuExpr_drop(nd->_match.cond);
        FuVec_drop_with_ptrs(nd->_match.arms, (FuDropFn)FuArm_drop);
        break;
    case ND_LOOP:
        FuLabel_drop(nd->_loop.label);
        FuBlock_drop(nd->_loop.block);
        break;
    case ND_WHILE:
        FuLabel_drop(nd->_while.label);
        FuExpr_drop(nd->_while.cond);
        FuBlock_drop(nd->_while.block);
        break;
    case ND_FOR:
        FuLabel_drop(nd->_for.label);
        FuPat_drop(nd->_for.pat);
        FuExpr_drop(nd->_for.expr);
        FuBlock_drop(nd->_for.block);
        break;
    case ND_TRY:
        FuBlock_drop(nd->_try.block);
        FuVec_drop_with_ptrs(nd->_try.arms, (FuDropFn)FuArm_drop);
        FuBlock_drop(nd->_try.finally);
        break;
    case ND_FN:
        FuIdent_drop(nd->_fn.ident);
        FuScope_drop(nd->_fn.scope);
        FuVec_drop_with_ptrs(nd->_fn.params, (FuDropFn)FuFnParam_drop);
        FuFnSig_drop(nd->_fn.sig);
        FuBlock_drop(nd->_fn.body);
        break;
    case ND_STRUCT:
        FuVariant_drop(nd->_struct.va);
        break;
    case ND_ENUM:
        FuIdent_drop(nd->_enum.ident);
        FuVec_drop_with_ptrs(nd->_enum.items, (FuDropFn)FuVariant_drop);
        break;
    case ND_UNION:
        FuVariant_drop(nd->_union.va);
        break;
    case ND_INTERFACE:
        FuIdent_drop(nd->_interface.ident);
        FuVec_drop(nd->_interface.supers);
        FuVec_drop_with_ptrs(nd->_interface.assocs, (FuDropFn)FuAssoc_drop);
        break;
    case ND_TY_ALIAS:
        FuIdent_drop(nd->_ty_alias.ident);
        break;
    case ND_EXTERN:
        FuLit_drop(nd->_extern.abi);
        FuVec_drop(nd->_extern.declares);
        break;
    case ND_EXTENSION:
        FuVec_drop_with_ptrs(nd->_extension.assocs, (FuDropFn)FuAssoc_drop);
        break;
    case ND_PKG:
        FuScope_drop(nd->_pkg.globals);
        FuScope_drop(nd->_pkg.builtins);
        FuVec_drop_with_ptrs(nd->_pkg.extern_pkgs, (FuDropFn)FuNode_drop);
        /* node ptr is drop in the context */
        FuVec_drop(nd->_pkg.items);
        break;
    default:
        FATAL1(NULL, "unimplemented node: `%s`", FuKind_node_cstr(nd->kd));
    }
    FuVec_drop_with_ptrs(nd->attrs, (FuDropFn)FuAttr_drop);
    FuMem_free(nd);
}

FuNode *FuNode_new_expr(FuCtx *ctx, FuExpr *expr) {
    FuNode *nd = FuNode_new(ctx, expr->sp, ND_EXPR);
    nd->_expr.expr = expr;
    return nd;
}

FuNode *FuNode_new_pkg(FuCtx *ctx, FuSpan *sp) {
    FuNode *nd = FuNode_new(ctx, sp, ND_PKG);
    FuScope *builtins = FuScope_new(ctx, NULL, 0);
    FuScope *globals = FuScope_new(ctx, builtins, 0);
    nd->_pkg.builtins = builtins;
    nd->_pkg.globals = globals;
    nd->_pkg.extern_pkgs = FuVec_new(sizeof(FuNode *));
    FuType_init_pkg_builtins(ctx, nd);
    return nd;
}

FuStr *FuNode_display_items(FuVec *items, fu_size_t indent) {
    FuStr *str = FuStr_new();
    fu_size_t len = FuVec_len(items);
    fu_size_t i;
    for (i = 0; i < len; i++) {
        FuNode *item = FuVec_get_ptr(items, i);
        FuStr_append(str, FuNode_display(item, indent));
    }
    return str;
}

FuStr *FuNode_display(FuNode *nd, fu_size_t indent) {
    FuStr *str = FuStr_new();
    FuStr_push_indent(str, indent);
    FuStr_push_utf8_format(str, "nid: %d\n", nd->nid);
    FuStr_push_indent(str, indent);
    FuStr_push_utf8_format(str, "kd: %s\n", FuKind_node_cstr(nd->kd));
    fu_size_t attrs_len = nd->attrs ? FuVec_len(nd->attrs) : 0;
    if (attrs_len) {
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "attrs:\n");
        fu_size_t i;
        for (i = 0; i < attrs_len; i++) {
            FuAttr *item = FuVec_get_ptr(nd->attrs, i);
            FuStr_append(str, FuAttr_display(item, indent + 1));
        }
    }
    switch (nd->kd) {
    case ND_EXPR:
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "expr:\n");
        FuStr_append(str, FuExpr_display(nd->_expr.expr, indent + 1));
        break;
    case ND_USE:
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_format(str, "vis: %s\n", FuKind_vis_cstr(nd->_use.vis));
        FuStr_append(str, FuUse_display(nd->_use.tree, indent + 1));
        break;
    case ND_STATIC:
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_format(str, "vis: %s\n", FuKind_vis_cstr(nd->_static.vis));
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "ident: ");
        FuStr_append(str, FuIdent_display(nd->_static.ident));
        FuStr_push_utf8_cstr(str, "\n");
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "ty: ");
        FuStr_append(str, FuType_display(nd->_static.ty));
        FuStr_push_utf8_cstr(str, "\n");
        if (nd->_static.init) {
            FuStr_push_indent(str, indent);
            FuStr_push_utf8_cstr(str, "init:\n");
            FuStr_append(str, FuExpr_display(nd->_static.init, indent + 1));
        }
        break;
    case ND_CONST:
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_format(str, "vis: %s\n", FuKind_vis_cstr(nd->_const.vis));
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "ident: ");
        FuStr_append(str, FuIdent_display(nd->_const.ident));
        FuStr_push_utf8_cstr(str, "\n");
        if (nd->_const.ty) {
            FuStr_push_indent(str, indent);
            FuStr_push_utf8_cstr(str, "ty: ");
            FuStr_append(str, FuType_display(nd->_const.ty));
            FuStr_push_utf8_cstr(str, "\n");
        }
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "init:\n");
        FuStr_append(str, FuExpr_display(nd->_const.init, indent + 1));
        break;
    case ND_LET:
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "pat:\n");
        FuStr_append(str, FuPat_display(nd->_let.pat, indent + 1));
        if (nd->_let.ty) {
            FuStr_push_indent(str, indent);
            FuStr_push_utf8_cstr(str, "ty: ");
            FuStr_append(str, FuType_display(nd->_let.ty));
            FuStr_push_utf8_cstr(str, "\n");
        }
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "init:\n");
        FuStr_append(str, FuExpr_display(nd->_let.init, indent + 1));
        break;
    case ND_ASSIGN:
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_format(str, "op: %s\n", FuKind_op_cstr(nd->_assign.op));
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "lexpr:\n");
        FuStr_append(str, FuExpr_display(nd->_assign.lexpr, indent + 1));
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "rexpr:\n");
        FuStr_append(str, FuExpr_display(nd->_assign.rexpr, indent + 1));
        break;
    case ND_BREAK:
        if (nd->_break.label) {
            FuStr_push_indent(str, indent);
            FuStr_push_utf8_cstr(str, "label: ");
            FuStr_append(str, FuLabel_display(nd->_break.label));
            FuStr_push_utf8_cstr(str, "\n");
        }
        break;
    case ND_CONTINUE:
        if (nd->_continue.label) {
            FuStr_push_indent(str, indent);
            FuStr_push_utf8_cstr(str, "label: ");
            FuStr_append(str, FuLabel_display(nd->_continue.label));
            FuStr_push_utf8_cstr(str, "\n");
        }
        break;
    case ND_YIELD:
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "expr:\n");
        FuStr_append(str, FuExpr_display(nd->_yield.expr, indent + 1));
        break;
    case ND_THROW:
        if (nd->_throw.expr) {
            FuStr_push_indent(str, indent);
            FuStr_push_utf8_cstr(str, "expr:\n");
            FuStr_append(str, FuExpr_display(nd->_throw.expr, indent + 1));
        }
        break;
    case ND_RETURN:
        if (nd->_return.expr) {
            FuStr_push_indent(str, indent);
            FuStr_push_utf8_cstr(str, "expr:\n");
            FuStr_append(str, FuExpr_display(nd->_return.expr, indent + 1));
        }
        break;
    case ND_BLOCK:
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_format(str, "is_unsafe: %d\n", nd->_block.is_unsafe);
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "block:\n");
        FuStr_append(str, FuBlock_display(nd->_block.block, indent + 1));
        break;
    case ND_IF:
        FuStr_push_indent(str, indent);
        if (nd->_if.cond) {
            FuStr_push_utf8_cstr(str, "cond:\n");
            FuStr_append(str, FuExpr_display(nd->_if.cond, indent + 1));
        } else {
            FuStr_push_utf8_cstr(str, "cond: else\n");
        }
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "block:\n");
        FuStr_append(str, FuBlock_display(nd->_if.block, indent + 1));
        if (nd->_if.next_if) {
            FuStr_push_indent(str, indent);
            FuStr_push_utf8_cstr(str, "next_if:\n");
            FuStr_append(str, FuNode_display(nd->_if.next_if, indent + 1));
        }
        break;
    case ND_MATCH: {
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "cond:\n");
        FuStr_append(str, FuExpr_display(nd->_match.cond, indent + 1));
        fu_size_t len = FuVec_len(nd->_match.arms);
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_format(str, "arms len: %d\n", len);
        FuStr_push_indent(str, indent);
        if (len > 0) {
            FuStr_push_utf8_cstr(str, "arms:\n");
            fu_size_t i;
            for (i = 0; i < len; i++) {
                FuArm *item = FuVec_get_ptr(nd->_match.arms, i);
                FuStr_append(str, FuArm_display(item, indent + 1));
            }
        }
        break;
    }
    case ND_LOOP:
        if (nd->_loop.label) {
            FuStr_push_indent(str, indent);
            FuStr_push_utf8_cstr(str, "label: ");
            FuStr_append(str, FuLabel_display(nd->_loop.label));
            FuStr_push_utf8_cstr(str, "\n");
        }
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "block:\n");
        FuStr_append(str, FuBlock_display(nd->_loop.block, indent + 1));
        break;
    case ND_WHILE:
        if (nd->_while.label) {
            FuStr_push_indent(str, indent);
            FuStr_push_utf8_cstr(str, "label: ");
            FuStr_append(str, FuLabel_display(nd->_while.label));
            FuStr_push_utf8_cstr(str, "\n");
        }
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "cond:\n");
        FuStr_append(str, FuExpr_display(nd->_while.cond, indent + 1));
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "block:\n");
        FuStr_append(str, FuBlock_display(nd->_while.block, indent + 1));
        break;
    case ND_FOR:
        if (nd->_for.label) {
            FuStr_push_indent(str, indent);
            FuStr_push_utf8_cstr(str, "label: ");
            FuStr_append(str, FuLabel_display(nd->_for.label));
            FuStr_push_utf8_cstr(str, "\n");
        }
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "pat:\n");
        FuStr_append(str, FuPat_display(nd->_for.pat, indent + 1));
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "expr:\n");
        FuStr_append(str, FuExpr_display(nd->_for.expr, indent + 1));
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "block:\n");
        FuStr_append(str, FuBlock_display(nd->_for.block, indent + 1));
        break;
    case ND_TRY: {
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "block:\n");
        FuStr_append(str, FuBlock_display(nd->_try.block, indent + 1));
        fu_size_t len = FuVec_len(nd->_try.arms);
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_format(str, "arms len: %d\n", len);
        if (len > 0) {
            FuStr_push_indent(str, indent);
            FuStr_push_utf8_cstr(str, "arms:\n");
            fu_size_t i;
            for (i = 0; i < len; i++) {
                FuArm *item = FuVec_get_ptr(nd->_try.arms, i);
                FuStr_append(str, FuArm_display(item, indent + 1));
            }
        }
        if (nd->_try.finally) {
            FuStr_push_indent(str, indent);
            FuStr_push_utf8_cstr(str, "finally:\n");
            FuStr_append(str, FuBlock_display(nd->_try.finally, indent + 1));
        }
        break;
    }
    case ND_FN: {
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_format(str, "vis: %s\n", FuKind_vis_cstr(nd->_fn.vis));
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "ident: ");
        FuStr_append(str, FuIdent_display(nd->_fn.ident));
        FuStr_push_utf8_cstr(str, "\n");
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_format(str, "vis: %s\n", FuKind_vis_cstr(nd->_fn.vis));
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "sig: ");
        FuStr_append(str, FuFnSig_display(nd->_fn.sig));
        FuStr_push_utf8_cstr(str, "\n");
        fu_size_t len = FuVec_len(nd->_fn.params);
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_format(str, "params len: %d\n", len);
        if (len > 0) {
            fu_size_t i;
            for (i = 0; i < len; i++) {
                FuFnParam *item = FuVec_get_ptr(nd->_fn.params, i);
                FuStr_append(str, FuFnParam_display(item, indent + 1));
            }
        }
        if (nd->_fn.body) {
            FuStr_push_indent(str, indent);
            FuStr_push_utf8_cstr(str, "body:\n");
            FuStr_append(str, FuBlock_display(nd->_fn.body, indent + 1));
        }
        break;
    }
    case ND_STRUCT:
        FuStr_append(str, FuVariant_display(nd->_struct.va, indent + 1));
        break;
    case ND_ENUM: {
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_format(str, "vis: %s\n", FuKind_vis_cstr(nd->_enum.vis));
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "ident:");
        FuStr_append(str, FuIdent_display(nd->_enum.ident));
        FuStr_push_utf8_cstr(str, "\n");
        fu_size_t len = FuVec_len(nd->_enum.items);
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_format(str, "items len: %d", len);
        FuStr_push_utf8_cstr(str, "\n");
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "items:\n");
        fu_size_t i;
        for (i = 0; i < len; i++) {
            FuVariant *item = FuVec_get_ptr(nd->_enum.items, i);
            FuStr_append(str, FuVariant_display(item, indent + 1));
        }
        break;
    }
    case ND_UNION:
        FuStr_append(str, FuVariant_display(nd->_union.va, indent + 1));
        break;
    case ND_INTERFACE: {
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_format(str, "vis: %s\n", FuKind_vis_cstr(nd->_interface.vis));
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_format(str, "is_unsafe: %d\n", nd->_interface.is_unsafe);
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "ident:");
        FuStr_append(str, FuIdent_display(nd->_interface.ident));
        FuStr_push_utf8_cstr(str, "\n");
        fu_size_t super_len = FuVec_len(nd->_interface.supers);
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_format(str, "super len: %d\n", super_len);
        fu_size_t i;
        for (i = 0; i < super_len; i++) {
            FuStr_push_indent(str, indent + 1);
            FuType *item = FuVec_get_ptr(nd->_interface.supers, i);
            FuStr_append(str, FuType_display(item));
            FuStr_push_utf8_cstr(str, "\n");
        }
        fu_size_t assoc_len = FuVec_len(nd->_interface.assocs);
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_format(str, "assoc len: %d\n", assoc_len);
        for (i = 0; i < assoc_len; i++) {
            FuAssoc *item = FuVec_get_ptr(nd->_interface.assocs, i);
            FuStr_append(str, FuAssoc_display(item, indent + 1));
        }
        break;
    }
    case ND_TY_ALIAS:
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_format(str, "vis: %s\n", FuKind_vis_cstr(nd->_ty_alias.vis));
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "ident:");
        FuStr_append(str, FuIdent_display(nd->_ty_alias.ident));
        FuStr_push_utf8_cstr(str, "\n");
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "ty: ");
        FuStr_append(str, FuType_display(nd->_ty_alias.ty));
        FuStr_push_utf8_cstr(str, "\n");
        break;
    case ND_EXTERN:
        if (nd->_extern.abi) {
            FuStr_push_indent(str, indent);
            FuStr_push_utf8_cstr(str, "abi:\n");
            FuStr_append(str, FuLit_display(nd->_extern.abi, indent + 1));
        }
        fu_size_t len = FuVec_len(nd->_extern.declares);
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_format(str, "declares len: %d\n", len);
        fu_size_t i;
        for (i = 0; i < len; i++) {
            FuNode *item = FuVec_get_ptr(nd->_extern.declares, i);
            FuStr_append(str, FuNode_display(item, indent + 1));
        }
        break;
    case ND_EXTENSION: {
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_format(str, "is_unsafe: %d\n", nd->_extension.is_unsafe);
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "ty: ");
        FuStr_append(str, FuType_display(nd->_extension.ty));
        FuStr_push_utf8_cstr(str, "\n");
        if (nd->_extension.interface) {
            FuStr_push_indent(str, indent);
            FuStr_push_utf8_cstr(str, "interface: ");
            FuStr_append(str, FuType_display(nd->_extension.interface));
            FuStr_push_utf8_cstr(str, "\n");
        }
        fu_size_t len = FuVec_len(nd->_extension.assocs);
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_format(str, "extension len: %d\n", len);
        for (i = 0; i < len; i++) {
            FuAssoc *item = FuVec_get_ptr(nd->_extension.assocs, i);
            FuStr_append(str, FuAssoc_display(item, indent + 1));
        }
        break;
    }
    case ND_PKG:
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "items:\n");
        FuStr_append(str, FuNode_display_items(nd->_pkg.items, indent + 1));
        break;
    default:
        FATAL1(NULL, "unimplemented node: `%s`", FuKind_node_cstr(nd->kd));
        break;
    }
    return str;
}
