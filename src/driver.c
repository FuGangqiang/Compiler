#include <errno.h>
#include <stdio.h>
#include <string.h>

#include "alloc.h"
#include "driver.h"
#include "log.h"
#include "parse.h"
#include "unix.h"

static void usage(int status) {
    /* clang-format off */
    fprintf(stderr,
"fuc: fu lang compiler\n"
"\n"
"Usage: fuc [OPTIONS] INPUT\n"
"\n"
"OPTIONS:\n"
"    -h, --help       Display this message\n"
"    --emit [raw-tokens|greed-tokens|parser-tokens|raw-ast|expended-ast|typed-ast|ir]\n"
"                     The type of output for the compiler to emit\n"
"    -o OUTPUT        Write output to <OUTPUT>\n"
"    --out-dir DIR    Write output to compiler-chosen filename in <DIR>\n"
);
    /* clang-format on */
    exit(status);
}

char *FuKind_emit_cstr(fu_emit_k kd) {
    switch (kd) {
/* clang-format off */
#define EMIT(kd, _doc) \
    case kd:          \
        return #kd;   \
        break;
/* clang-format on */
#include "driver_emit.def"
#undef EMIT
    default:
        /* can not be here */
        return "";
        break;
    }
}

FuConfig *FuConfig_new() {
    return FuMem_new(FuConfig);
}

void FuConfig_drop(FuConfig *cfg) {
    if (!cfg) {
        return;
    }
    FuStr_drop(cfg->input_fpath);
    FuStr_drop(cfg->out_dir);
    FuStr_drop(cfg->out_file);
    if (cfg->out) {
        fclose(cfg->out);
    }
    FuMem_free(cfg);
}

FuStr *FuConfig_dispaly(FuConfig *cfg) {
    FuStr *str = FuStr_new();
    FuStr_push_utf8_cstr(str, "cfg.input_fpath = ");
    FuStr_append(str, FuStr_clone(cfg->input_fpath));
    FuStr_push_utf8_cstr(str, "\n");
    FuStr_push_utf8_cstr(str, "cfg.out_dir = ");
    FuStr_append(str, FuStr_clone(cfg->out_dir));
    FuStr_push_utf8_cstr(str, "\n");
    FuStr_push_utf8_cstr(str, "cfg.out_file = ");
    FuStr_append(str, FuStr_clone(cfg->out_file));
    FuStr_push_utf8_cstr(str, "\n");
    FuStr_push_utf8_format(str, "cfg.emit = %s\n", FuKind_emit_cstr(cfg->emit));
    return str;
}

void FuConfig_init(FuConfig *cfg) {
    if (cfg->emit == EMIT_NONE) {
        cfg->emit = EMIT_BIN;
    }

    if (!cfg->input_fpath) {
        fprintf(stderr, "Error: `INPUT` filename must be sepecified\n");
        usage(1);
    }
    FuStr *fu_ext = FuStr_from_utf8_cstr(".fu");
    if (!FuStr_ends_with(cfg->input_fpath, fu_ext)) {
        fprintf(stderr, "Error: `INPUT` filename must end with `.fu` extension\n");
        usage(1);
    }
    FuStr_drop(fu_ext);

    if (!cfg->out_file) {
        FuStr *fname = FuStr_path_fname(cfg->input_fpath);
        if (cfg->emit == EMIT_RAW_TOKENS) {
            cfg->out_file = FuStr_path_with_extension(fname, "tokens.raw");
        } else if (cfg->emit == EMIT_GREED_TOKENS) {
            cfg->out_file = FuStr_path_with_extension(fname, "tokens.greed");
        } else if (cfg->emit == EMIT_PARSER_TOKENS) {
            cfg->out_file = FuStr_path_with_extension(fname, "tokens.parser");
        } else if (cfg->emit == EMIT_RAW_AST) {
            cfg->out_file = FuStr_path_with_extension(fname, "tokens.ast.raw");
        } else if (cfg->emit == EMIT_EXPENDED_AST) {
            cfg->out_file = FuStr_path_with_extension(fname, "tokens.ast.expended");
        } else if (cfg->emit == EMIT_TYPED_AST) {
            cfg->out_file = FuStr_path_with_extension(fname, "tokens.ast.typed");
        } else if (cfg->emit == EMIT_IR) {
            cfg->out_file = FuStr_path_with_extension(fname, "ir");
        } else if (cfg->emit == EMIT_BIN) {
            cfg->out_file = FuStr_path_file_stem(fname);
        }
        FuStr_drop(fname);
    } else {
        if (FuStr_find(cfg->out_file, '/', NULL)) {
            fprintf(stderr, "Error: `OUTPUT` filename should not contain `/`");
            usage(1);
        }
    }
    if (!cfg->out_dir) {
        cfg->out_dir = FuStr_get_cur_dir();
    }
    FuStr *out_fpath = FuStr_clone(cfg->out_dir);
    FuStr_path_join(out_fpath, FuStr_clone(cfg->out_file));
    char path[4096];
    FuStr_to_cstr(out_fpath, path, 4096);
    cfg->out = fopen(path, "w");
    if (!cfg->out) {
        FATAL2(NULL, "can not open file: `%s`: %s", path, strerror(errno));
    }
    FuStr_drop(out_fpath);
}

static FuConfig *FuDriver_parse_config(int argc, char **argv) {
    FuConfig *cfg = FuConfig_new();

    /* clang-format off */
#define CHECK_OPT_EXIST(opt)                                         \
    if (opt) {                                                       \
        fprintf(stderr, "Error: duplicated options: %s\n", argv[i]); \
        usage(1);                                                     \
    }
    /* clang-format on */

    fu_size_t i;
    for (i = 1; i < argc; i++) {
        fu_bool_t is_last_arg = i == argc - 1;

        if (!strcmp(argv[i], "-h")) {
            usage(0);
        } else if (!strcmp(argv[i], "--help")) {
            usage(0);
        } else if (!strcmp(argv[i], "-O") && !is_last_arg) {
            CHECK_OPT_EXIST(cfg->out_file);
            cfg->out_file = FuStr_from_utf8_cstr(argv[++i]);
        } else if (!strcmp(argv[i], "--out-dir") && !is_last_arg) {
            CHECK_OPT_EXIST(cfg->out_dir);
            cfg->out_dir = FuStr_from_utf8_cstr(argv[++i]);
        } else if (!strcmp(argv[i], "--emit") && !is_last_arg) {
            CHECK_OPT_EXIST(cfg->emit);
            char *arg = argv[++i];
            if (!strcmp(arg, "raw-tokens")) {
                cfg->emit = EMIT_RAW_TOKENS;
            } else if (!strcmp(arg, "greed-tokens")) {
                cfg->emit = EMIT_GREED_TOKENS;
            } else if (!strcmp(arg, "parser-tokens")) {
                cfg->emit = EMIT_PARSER_TOKENS;
            } else if (!strcmp(arg, "raw-ast")) {
                cfg->emit = EMIT_RAW_AST;
            } else if (!strcmp(arg, "expended-ast")) {
                cfg->emit = EMIT_EXPENDED_AST;
            } else if (!strcmp(arg, "typed-ast")) {
                cfg->emit = EMIT_TYPED_AST;
            } else if (!strcmp(arg, "ir")) {
                cfg->emit = EMIT_IR;
            } else {
                fprintf(stderr, "ERROR: unrecognized argument for `--emit` option.\n\n");
                usage(1);
            }
        } else if (argv[i][0] == '-') {
            fprintf(stderr, "ERROR: unrecognized option or bad number of args for: `%s`\n", argv[i]);
            usage(1);
        } else {
            if (is_last_arg) {
                cfg->input_fpath = FuStr_abs_path(argv[i]);
            } else {
                fprintf(stderr, "ERROR: unrecognized option or bad number of args for: `%s`\n", argv[i]);
                usage(1);
            }
        }
    }

#undef CHECK_OPT_EXIST
    return cfg;
}

static void FuDriver_emit_raw_tokens(FuPkg *pkg) {
    FuLexer *l = FuLexer_new(pkg);
    FuLexer_for_file(l, FuStr_clone(pkg->fpath));

    FuStr *dump = FuStr_new();
    while (1) {
        FuToken tok = FuLexer_get_token(l);
        if (FuToken_is_eof(tok)) {
            break;
        }
        FuStr_append(dump, FuToken_debug(tok));
        FuStr_push(dump, '\n');
    }
    FuStr_print(dump, pkg->cfg->out);

    FuStr_drop(dump);
    FuLexer_drop(l);
}

static void FuDriver_emit_greed_tokens(FuPkg *pkg) {
    FuStr *dump = FuStr_new();

    FuParser *p = FuParser_new(pkg);
    p->tok_level = TOK_LEVEL_OPS;
    while (1) {
        FuToken tok = FuParser_get_token(p);
        if (tok.kd == TOK_EOF) {
            break;
        }
        FuStr_append(dump, FuToken_debug(tok));
        FuStr_push(dump, '\n');
    }
    FuStr_print(dump, pkg->cfg->out);

    FuStr_drop(dump);
    FuParser_drop(p);
}

int FuDriver_main(int argc, char **argv) {
    FuConfig *cfg = FuDriver_parse_config(argc, argv);
    FuConfig_init(cfg);

    /*
    FuStr* cfg_str= FuConfig_dispaly(cfg);
    FuStr_print(cfg_str, stderr);
    FuStr_drop(cfg_str);
    */

    FuParser *p = NULL;
    FuPkg *pkg = FuPkg_new(cfg);
    FuPkg_init(pkg);

    if (cfg->emit == EMIT_RAW_TOKENS) {
        FuDriver_emit_raw_tokens(pkg);
        goto end;
    }

    if (cfg->emit == EMIT_GREED_TOKENS) {
        FuDriver_emit_greed_tokens(pkg);
        goto end;
    }

    p = FuParser_new(pkg);
    FuParser_parse_pkg(p);

    if (cfg->emit == EMIT_PARSER_TOKENS) {
        /* tokens emit by FuParser_bump */
        goto end;
    }
    if (cfg->emit == EMIT_RAW_AST) {
        FuStr *dump = FuPkg_display(pkg, 0);
        FuStr_print(dump, cfg->out);
        FuStr_drop(dump);
        goto end;
    }
    if (cfg->emit >= EMIT_EXPENDED_AST) {
        fprintf(stderr, "unimplemented macro expension");
        goto error;
    }

end:
    if (p) {
        FuParser_drop(p);
    }
    FuPkg_drop(pkg);
    return 0;
error:
    if (p) {
        FuParser_drop(p);
    }
    FuPkg_drop(pkg);
    return 1;
}
