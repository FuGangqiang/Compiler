#ifndef FU_DRIVER_H
#define FU_DRIVER_H

#include <stdio.h>

#include "str.h"

typedef enum fu_emit_k fu_emit_k;
enum fu_emit_k {
#define EMIT(kd, _doc) kd,
#include "driver_emit.def"
#undef EMIT
    _EMIT_LAST_UNUSED
};

char *FuKind_emit_cstr(fu_emit_k kd);

typedef struct FuConfig FuConfig;
struct FuConfig {
    FuStr *input_fpath;
    FuStr *out_dir;
    FuStr *out_file;
    fu_emit_k emit;

    FILE *out;
};

FuConfig *FuConfig_new();
void FuConfig_init(FuConfig *cfg);
FuStr *FuConfig_dispaly(FuConfig *cfg);
void FuConfig_drop(FuConfig *cfg);

int FuDriver_main(int argc, char **argv);

#endif /* FU_DRIVER_H */
