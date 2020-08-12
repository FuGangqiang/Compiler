#include "alloc.h"
#include "parse.h"

FuScope *FuScope_new(FuContext *ctx, FuScope *super, FuSymbol name) {
    FuScope *scp = FuMem_new(FuScope);
    scp->ctx = ctx;
    scp->super = super;
    scp->name = name;

    FuMap *types = FuMap_new(sizeof(FuSymbol), sizeof(fu_size_t), (FuEqFn)FuSymbol_eq, (FuHashFn)FuSymbol_hash);
    scp->types = types;
    return scp;
}

void FuScope_drop(FuScope *scp) {
    FuMap_drop(scp->types);
    FuMem_free(scp);
}

FuType *FuScope_get_type(FuScope *scp, FuSymbol name) {
    fu_size_t *tid = NULL;
    do {
        tid = FuMap_get(scp->types, &name);
        if (tid) {
            break;
        }
        scp = scp->super;
    } while (scp);
    return FuContext_get_type(scp->ctx, *tid);
}
