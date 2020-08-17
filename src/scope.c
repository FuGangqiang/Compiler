#include "alloc.h"
#include "parse.h"

FuScope *FuScope_new(FuCtx *ctx, FuScope *super, fu_sym_t name) {
    FuScope *scp = FuMem_new(FuScope);
    scp->ctx = ctx;
    scp->super = super;
    scp->name = name;

    FuMap *types = FuMap_new(sizeof(fu_sym_t), sizeof(fu_size_t), (FuEqFn)FuId_eq, (FuHashFn)FuId_hash);
    scp->types = types;
    return scp;
}

void FuScope_drop(FuScope *scp) {
    FuMap_drop(scp->types);
    FuMem_free(scp);
}

FuType *FuScope_get_type(FuScope *scp, fu_sym_t name) {
    fu_size_t *tid = NULL;
    do {
        tid = FuMap_get(scp->types, &name);
        if (tid) {
            break;
        }
        scp = scp->super;
    } while (scp);
    return FuCtx_get_type(scp->ctx, *tid);
}
