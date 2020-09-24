#ifndef FU_UNIX_H
#define FU_UNIX_H

#include "str.h"

FuStr *FuStr_abs_path(char *fpath);
FuStr *FuStr_rel_path(FuStr *real_path, FuStr *start);
FuStr *FuStr_path_dir(FuStr *path);
FuStr *FuStr_path_fname(FuStr *path);
FuStr *FuStr_path_file_stem(FuStr *path);
FuStr *FuStr_path_with_extension(FuStr *path, char *extension);
FuStr *FuStr_get_cur_dir();
void FuStr_path_join(FuStr *dir, FuStr *name);

#endif /* FU_UNIX_H */
