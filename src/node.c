#include "alloc.h"
#include "parse.h"

FuNode *FuNode_new(FuContext *ctx, FuSpan span, fu_node_k kind) {
    FuNode *node = FuMem_new(FuNode);
    node->span = span;
    node->kd = kind;
    node->nid = FuVec_len(ctx->nodes);
    FuVec_push_ptr(ctx->nodes, node);
    return node;
}

void FuNode_drop(FuNode *nd) {
    FuMem_free(nd);
}

FuStr *FuNode_display(FuNode *nd, fu_size_t indent) {
    FuStr *str = FuStr_new();
    return str;
}
