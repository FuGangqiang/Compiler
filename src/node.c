#include "alloc.h"
#include "error.h"
#include "parse.h"

static void FuStr_push_indent(FuStr *str, fu_size_t n) {
    fu_size_t i;
    for (i = 0; i < n * 2; i++) {
        FuStr_push(str, ' ');
    }
}

FuLit *FuLit_new(FuSpan span, fu_lit_k kind) {
    FuLit *lit = FuMem_new(FuLit);
    lit->span = span;
    lit->kd = kind;
    return lit;
}

void FuLit_drop(FuLit *lit) {
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
        FATAL(NULL, "unimplemented");
        break;
    }
    return str;
}

FuNode *FuNode_new(FuContext *ctx, FuSpan span, fu_node_k kind) {
    FuNode *node = FuMem_new(FuNode);
    node->span = span;
    node->kd = kind;
    node->nid = FuVec_len(ctx->nodes);
    FuVec_push_ptr(ctx->nodes, node);
    return node;
}

void FuNode_drop(FuNode *nd) {
    switch (nd->kd) {
    case ND_LIT:
        FuLit_drop(nd->_lit.lit);
        break;
    default:
        FATAL(NULL, "unimplemented!");
    }
    FuMem_free(nd);
}

FuStr *FuNode_display(FuNode *nd, fu_size_t indent) {
    FuStr *str = FuStr_new();
    FuStr_push_indent(str, indent);
    FuStr_push_utf8_format(str, "nid: %d\n", nd->nid);
    FuStr_push_indent(str, indent);
    FuStr_push_utf8_format(str, "kd: %s\n", FuKind_node_cstr(nd->kd));
    switch (nd->kd) {
    case ND_LIT: {
        FuStr *lit = FuLit_display(nd->_lit.lit, indent + 1);
        FuStr_append(str, lit);
        break;
    }
    default:
        FATAL(NULL, "unimplemented");
        break;
    }
    return str;
}
