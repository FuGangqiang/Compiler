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
    FuPathItem *item = FuVec_get_ptr(path->segments, 0);
    FuStr_append(str, FuPathItem_display(item));
    fu_size_t len = FuVec_len(path->segments);
    fu_size_t i;
    for (i = 1; i < len; i++) {
        FuStr_push_utf8_cstr(str, "::");
        item = FuVec_get_ptr(path->segments, 0);
        FuStr_append(str, FuPathItem_display(item));
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
        FATAL1(lit->sp, "unimplemented, lit: %s\n", FuKind_lit_cstr(lit->kd));
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
    FuExpr *expr = FuExpr_new(path->sp, EXPR_PATH);
    expr->_path.anno = anno;
    expr->_path.path = path;
    return expr;
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
    case EXPR_TUPLE:
        FuVec_drop_with_ptrs(expr->_tuple.fields, (FuDropFn)FuExpr_drop);
        break;
    case EXPR_UNARY:
        FuExpr_drop(expr->_unary.expr);
        break;
    case EXPR_BINARY:
        FuExpr_drop(expr->_binary.lexpr);
        FuExpr_drop(expr->_binary.rexpr);
        break;
    default:
        FATAL1(expr->sp, "unimplemented: %s", FuKind_expr_cstr(expr->kd));
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
    if (expr->kd != EXPR_ERR) {
        FuStr_push_indent(str, indent);
    }
    switch (expr->kd) {
    case EXPR_ERR:
        break;
    case EXPR_LIT:
        FuStr_push_utf8_cstr(str, "lit:\n");
        FuStr_append(str, FuLit_display(expr->_lit, indent + 1));
        break;
    case EXPR_PATH:
        FuStr_push_utf8_cstr(str, "path: ");
        /* todo: expr->_path.anno */
        FuStr_append(str, FuPath_display(expr->_path.path));
        break;
    case EXPR_TUPLE: {
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
    case EXPR_UNARY:
        FuStr_push_utf8_format(str, "op: %s\n", FuKind_op_cstr(expr->_binary.op));
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "expr:\n");
        FuStr_append(str, FuExpr_display(expr->_unary.expr, indent + 1));
        break;
    case EXPR_BINARY:
        FuStr_push_utf8_format(str, "op: %s\n", FuKind_op_cstr(expr->_binary.op));
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "lexpr:\n");
        FuStr_append(str, FuExpr_display(expr->_binary.lexpr, indent + 1));
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "rexpr:\n");
        FuStr_append(str, FuExpr_display(expr->_binary.rexpr, indent + 1));
        break;
    default:
        FATAL1(expr->sp, "unimplemented: %s", FuKind_expr_cstr(expr->kd));
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
    case ND_STATIC:
        FuExpr_drop(nd->_static.init_expr);
        FuIdent_drop(nd->_static.ident);
        break;
    case ND_CONST:
        FuExpr_drop(nd->_const.init_expr);
        FuIdent_drop(nd->_const.ident);
        break;
    case ND_PKG:
        FuScope_drop(nd->_pkg.globals);
        FuScope_drop(nd->_pkg.builtins);
        FuVec_drop_with_ptrs(nd->_pkg.extern_pkgs, (FuDropFn)FuNode_drop);
        /* node ptr is drop in the context */
        FuVec_drop(nd->_pkg.items);
        break;
    default:
        FATAL1(NULL, "unimplemented: %s", FuKind_node_cstr(nd->kd));
    }
    if (nd->attrs) {
        /* todo: attr drop */
        FuVec_drop(nd->attrs);
    }
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
    FuStr_push_indent(str, indent);
    switch (nd->kd) {
    case ND_EXPR:
        FuStr_push_utf8_cstr(str, "expr:\n");
        FuStr_append(str, FuExpr_display(nd->_expr.expr, indent + 1));
        break;
    case ND_STATIC:
        FuStr_push_utf8_cstr(str, "ident:");
        FuStr_append(str, FuIdent_display(nd->_static.ident));
        FuStr_push_utf8_cstr(str, "\n");
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "init_expr:\n");
        FuStr_append(str, FuExpr_display(nd->_static.init_expr, indent + 1));
        break;
    case ND_CONST:
        FuStr_push_utf8_cstr(str, "ident:");
        FuStr_append(str, FuIdent_display(nd->_const.ident));
        FuStr_push_utf8_cstr(str, "\n");
        FuStr_push_indent(str, indent);
        FuStr_push_utf8_cstr(str, "init_expr:\n");
        FuStr_append(str, FuExpr_display(nd->_const.init_expr, indent + 1));
        break;
    case ND_PKG:
        FuStr_push_utf8_cstr(str, "items:\n");
        FuStr_append(str, FuNode_display_items(nd->_pkg.items, indent + 1));
        break;
    default:
        FATAL1(NULL, "unimplemented: %s", FuKind_node_cstr(nd->kd));
        break;
    }
    return str;
}
