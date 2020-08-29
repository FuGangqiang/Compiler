#ifndef FU_PARSE_H
#define FU_PARSE_H

#include "bytes.h"
#include "char.h"
#include "def.h"
#include "map.h"
#include "set.h"
#include "str.h"
#include "vec.h"

#define DUMP_INDENT_WIDTH 2

typedef fu_size_t fu_id_t;
typedef fu_id_t fu_nid_t; /* node id */
typedef fu_id_t fu_tid_t; /* type id */
typedef fu_id_t fu_sym_t; /* symbol id */

typedef enum fu_op_assoc_k fu_op_assoc_k;
typedef enum fu_op_fix_k fu_op_fix_k;
typedef enum fu_op_k fu_op_k;
typedef enum fu_ty_op_k fu_ty_op_k;

typedef enum fu_arm_k fu_arm_k;
typedef enum fu_attr_k fu_attr_k;
typedef enum fu_expr_k fu_expr_k;
typedef enum fu_field_k fu_field_k;
typedef enum fu_ge_arg_k fu_ge_arg_k;
typedef enum fu_ge_param_k fu_ge_param_k;
typedef enum fu_keyword_k fu_keyword_k;
typedef enum fu_lit_k fu_lit_k;
typedef enum fu_log_k fu_log_k;
typedef enum fu_node_k fu_node_k;
typedef enum fu_pat_k fu_pat_k;
typedef enum fu_token_k fu_token_k;
typedef enum fu_type_k fu_type_k;
typedef enum fu_use_k fu_use_k;
typedef enum fu_variant_k fu_variant_k;
typedef enum fu_vis_k fu_vis_k;

typedef struct FuCtx FuCtx;
typedef struct FuLog FuLog;
typedef struct FuSpan FuSpan;
typedef struct FuToken FuToken;
typedef struct FuLexer FuLexer;
typedef struct FuScope FuScope;
typedef struct FuParser FuParser;

typedef struct FuLit FuLit;
typedef struct FuPat FuPat;
typedef struct FuExpr FuExpr;
typedef struct FuNode FuNode;
typedef struct FuType FuType;

typedef struct FuAnnoSelf FuAnnoSelf;
typedef struct FuArm FuArm;
typedef struct FuAttr FuAttr;
typedef struct FuBlock FuBlock;
typedef struct FuFieldInit FuFieldInit;
typedef struct FuFnParam FuFnParam;
typedef struct FuFnSig FuFnSig;
typedef struct FuGeArg FuGeArg;
typedef struct FuGeBound FuGeBound;
typedef struct FuGeneric FuGeneric;
typedef struct FuGeParam FuGeParam;
typedef struct FuIdent FuIdent;
typedef struct FuLabel FuLabel;
typedef struct FuMacroCall FuMacroCall;
typedef struct FuPath FuPath;
typedef struct FuPathItem FuPathItem;
typedef struct FuPkg FuPkg;
typedef struct FuTokTree FuTokTree;
typedef struct FuUse FuUse;

fu_bool_t FuId_eq(fu_id_t *id1, fu_id_t *id2);
fu_size_t FuId_hash(fu_id_t *id);

/* operator precedence */
typedef fu_size_t fu_op_prec_t;

/* operator associativity */
enum fu_op_assoc_k {
    OP_PP, /* require parentheses */
    OP_LR, /* left to right */
    OP_RL, /* right to left */
};

/* operator type */
enum fu_op_fix_k {
    /* `..`, `..=` */
    OP_ALLFIX,
    OP_PREFIX,
    OP_INFIX,
    OP_SUFFIX,
};

/* must defined before fu_token_k */
enum fu_op_k {
#define OP(kd, _prec, _assoc, _ty, _doc) kd,
#include "node_op.def"
#undef OP
    _OP_LAST_UNUSED
};

fu_op_prec_t FuOp_precedence(fu_op_k kd);
fu_bool_t FuOp_is_unary(fu_op_k kd);
fu_bool_t FuOp_is_binary(fu_op_k kd);

enum fu_ty_op_k {
#define TYPE_OP(kd, _prec, _assoc, _ty, _doc) kd,
#include "type_op.def"
#undef TYPE_OP
    _TY_OP_LAST_UNUSED
};

fu_op_prec_t FuTyOp_precedence(fu_ty_op_k kd);

enum fu_log_k {
#define LOG(kd, _doc) kd,
#include "log.def"
#undef LOG
    _LOG_LAST_UNUSED
};

enum fu_arm_k {
#define ARM(kd, _doc) kd,
#include "node_arm.def"
#undef ARM
    _ARM_LAST_UNUSED
};

enum fu_attr_k {
#define ATTR(kd, _doc) kd,
#include "node_attr.def"
#undef ATTR
    _ATTR_LAST_UNUSED
};

enum fu_expr_k {
#define EXPR(kd, _doc) kd,
#include "node_expr.def"
#undef EXPR
    _EXPR_LAST_UNUSED
};

enum fu_field_k {
#define FIELD(kd, _doc) kd,
#include "node_field.def"
#undef FIELD
    _FLD_ARG_LAST_UNUSED
};

enum fu_ge_arg_k {
#define GE_ARG(kd, _doc) kd,
#include "node_ge_arg.def"
#undef GE_ARG
    _GE_ARG_LAST_UNUSED
};

enum fu_ge_param_k {
#define GE_PARAM(kd, _doc) kd,
#include "node_ge_param.def"
#undef GE_PARAM
    _GE_PARAM_LAST_UNUSED,
};

enum fu_keyword_k {
#define KEYWORD(kd, _doc) kd,
#include "keyword.def"
#undef KEYWORD
    _KW_LAST_UNUSED,
};

enum fu_lit_k {
#define LIT(kd, _doc) kd,
#include "node_lit.def"
#undef LIT
    _LIT_LAST_UNUSED
};

enum fu_node_k {
#define NODE(kd, _doc) kd,
#include "node.def"
#undef NODE
    _ND_LAST_UNUSED
};

enum fu_pat_k {
#define PAT(kd, _doc) kd,
#include "node_pat.def"
#undef PAT
    _PAT_LAST_UNUSED
};

enum fu_token_k {
#define TOKEN(kd, _doc) kd,
#include "token.def"
#undef TOKEN
    _TOK_LAST_UNUSED
};

enum fu_type_k {
#define TYPE(kd, _doc) kd,
#include "type.def"
#undef TYPE
    _TY_LAST_UNUSED,
};

enum fu_use_k {
#define USE(kd, _doc) kd,
#include "node_use.def"
#undef USE
    _USE_LAST_UNUSE
};

enum fu_variant_k {
#define VARIANT(kd, _doc) kd,
#include "node_variant.def"
#undef VARIANT
    _VARIANT_LAST_UNUSE
};

enum fu_vis_k {
#define VIS(kd, _doc) kd,
#include "node_vis.def"
#undef VIS
    _VIS_LAST_UNUSE
};

char *FuKind_arm_cstr(fu_arm_k kd);
char *FuKind_attr_cstr(fu_attr_k kd);
char *FuKind_expr_cstr(fu_expr_k kd);
char *FuKind_field_cstr(fu_field_k kd);
char *FuKind_ge_arg_cstr(fu_ge_arg_k kd);
char *FuKind_ge_param_cstr(fu_ge_param_k kd);
char *FuKind_keyword_cstr(fu_keyword_k kd);
char *FuKind_lit_cstr(fu_lit_k kd);
char *FuKind_log_cstr(fu_log_k kd);
char *FuKind_node_cstr(fu_node_k kd);
char *FuKind_op_cstr(fu_op_k kd);
char *FuKind_pat_cstr(fu_pat_k kd);
char *FuKind_token_cstr(fu_token_k kd);
char *FuKind_ty_op_cstr(fu_ty_op_k kd);
char *FuKind_type_cstr(fu_type_k kd);
char *FuKind_use_cstr(fu_use_k kd);
char *FuKind_variant_cstr(fu_variant_k kd);
char *FuKind_vis_cstr(fu_vis_k kd);

struct FuSpan {
    FuCtx *ctx;
    fu_sym_t fpath;
    fu_size_t start;
    fu_size_t len;
    fu_size_t line;
    fu_size_t column;
    fu_size_t offset; /* offset in token */
};

FuSpan *FuSpan_new(FuCtx *ctx, fu_sym_t fpath, fu_size_t start, fu_size_t len, fu_size_t line, fu_size_t column);
void FuSpan_init(FuSpan *sp, FuCtx *ctx, fu_sym_t fpath, fu_size_t start, fu_size_t len, fu_size_t line,
                 fu_size_t column);
void FuSpan_drop(FuSpan *sp);

FuSpan *FuSpan_offset(FuSpan *sp, fu_size_t offset);
FuSpan *FuSpan_unintern_join(FuSpan *sp1, FuSpan *sp2);
FuSpan *FuSpan_join(FuSpan *sp1, FuSpan *sp2);

FuStr *FuSpan_display(FuSpan *sp);
FuStr *FuSpan_line(FuSpan *sp);
FuStr *FuSpan_content(FuSpan *sp);

/* compiler context */
struct FuCtx {
    /* intern symbols */
    /* FuStr* */
    FuSet *symbols;

    /* intern lexer file context */
    /* fpath sym -> fcontent no*/
    FuMap *fmap;
    FuVec *fcontents;

    /* ast spans */
    FuVec *spans;

    /* ast nodes */
    FuVec *nodes;

    /* types */
    FuVec *types;
};

FuCtx *FuCtx_new();
void FuCtx_init(FuCtx *ctx);
void FuCtx_drop(FuCtx *ctx);

/* intern symbol */
fu_sym_t FuCtx_intern_symbol(FuCtx *ctx, FuStr *symbol);
FuStr *FuCtx_get_symbol(FuCtx *ctx, fu_sym_t sym);

/* intern file content */
void FuCtx_intern_file(FuCtx *ctx, fu_sym_t fpath, FuStr *fcontent);
FuStr *FuCtx_get_file(FuCtx *ctx, fu_sym_t fpath);

/* context spans */
void FuCtx_intern_span(FuCtx *ctx, FuSpan *sp);

/* context types */
fu_tid_t FuCtx_push_type(FuCtx *ctx, FuType *ty);
FuType *FuCtx_get_type(FuCtx *ctx, fu_tid_t tid);

struct FuToken {
    fu_token_k kd;
    FuSpan *sp;
    union {
        fu_sym_t sym;
        struct {
            fu_sym_t sym;
            fu_bool_t terminated;
        } _byte;
        struct {
            fu_sym_t sym;
            fu_bool_t terminated;
        } _char;
        struct {
            fu_sym_t sym;
            fu_size_t base;
            fu_bool_t empty_int;
            fu_size_t suffix_start;
        } _int;
        struct {
            fu_sym_t sym;
            fu_size_t base;
            fu_bool_t empty_exponent;
            fu_size_t suffix_start;
        } _float;
        struct {
            fu_sym_t sym;
            fu_size_t n_hashes;
            /* 是否有第一个引号, todo: 查看是否需要这个属性 */
            fu_bool_t started;
            fu_size_t prefix_ignore;
            fu_bool_t terminated;
        } _str;
    };
};

FuToken FuToken_new(fu_token_k kd, FuSpan *sp);
FuToken FuToken_new_doc_comment(FuSpan *sp, fu_sym_t sym);
FuToken FuToken_new_keyword(FuSpan *sp, fu_sym_t sym);
FuToken FuToken_new_ident(FuSpan *sp, fu_sym_t sym);
FuToken FuToken_new_raw_ident(FuSpan *sp, fu_sym_t sym);
FuToken FuToken_new_macro(FuSpan *sp, fu_sym_t sym);
FuToken FuToken_new_lable(FuSpan *sp, fu_sym_t sym);

FuToken FuToken_new_lit_int(FuSpan *sp, fu_sym_t sym, fu_size_t base, fu_bool_t empty_int, fu_size_t suffix_start);
FuToken FuToken_new_lit_float(FuSpan *sp, fu_sym_t sym, fu_size_t base, fu_bool_t empty_exponent,
                              fu_size_t suffix_start);
FuToken FuToken_new_lit_char(FuSpan *sp, fu_sym_t sym, fu_bool_t terminated);
FuToken FuToken_new_lit_byte(FuSpan *sp, fu_sym_t sym, fu_bool_t terminated);
FuToken FuToken_new_lit_str(fu_token_k kd, FuSpan *sp, fu_sym_t sym, fu_size_t n_hashes, fu_bool_t started,
                            fu_size_t prefix_ignore, fu_bool_t terminated);

fu_bool_t FuToken_is_eof(FuToken tok);
fu_bool_t FuToken_is_keyword(FuToken tok);
fu_bool_t FuToken_is_ident(FuToken tok);
fu_bool_t FuToken_is_open_delim(FuToken tok);
fu_bool_t FuToken_is_close_delim(FuToken tok);
fu_bool_t FuToken_is_match_delim(FuToken open, FuToken close);
fu_bool_t FuToken_is_assign(FuToken tok);
fu_bool_t FuToken_is_lit(FuToken tok);
fu_bool_t FuToken_is_outer_doc_comment(FuToken tok);
fu_bool_t FuToken_is_blank(FuToken tok);

FuLit *FuToken_to_lit_nil(FuToken tok);
FuLit *FuToken_to_lit_bool(FuToken tok);
FuLit *FuToken_to_lit_byte(FuToken tok);
FuLit *FuToken_to_lit_byte_str(FuToken tok);
FuLit *FuToken_to_lit_char(FuToken tok);
FuLit *FuToken_to_lit_str(FuToken tok);
FuLit *FuToken_to_lit_int(FuToken tok);
FuLit *FuToken_to_lit_float(FuToken tok);
FuLit *FuToken_to_lit_format_str(FuToken tok);
FuIdent *FuToken_index_to_ident(FuToken tok);

fu_bool_t FuToken_to_assign_op(FuToken tok, fu_op_k *op);
fu_bool_t FuToken_to_prefix_op(FuToken tok, fu_op_k *op);
fu_bool_t FuToken_to_infix_op(FuToken tok, fu_op_k *op);
fu_bool_t FuToken_to_suffix_op(FuToken tok, fu_op_k *op);
fu_bool_t FuToken_to_prefix_ty_op(FuToken tok, fu_ty_op_k *op);
fu_bool_t FuToken_to_infix_ty_op(FuToken tok, fu_ty_op_k *op);
fu_bool_t FuToken_to_suffix_ty_op(FuToken tok, fu_ty_op_k *op);

fu_size_t FuToken_left_skip_count(FuToken tok);

FuStr *FuToken_display(FuToken tok);
char *FuToken_kind_csr(FuToken tok);

struct FuLexer {
    FuCtx *ctx;
    fu_sym_t fpath;
    /* interned fcontent */
    FuStr *chars;
    fu_size_t tok_line;
    fu_size_t tok_column;
    fu_size_t cur_line;
    fu_size_t cur_column;
    fu_size_t cursor;
    /* for parseing multi-char ops */
    FuVec *tok_buf;
};

FuLexer *FuLexer_new(FuCtx *ctx);
void FuLexer_drop(FuLexer *l);

void FuLexer_for_file(FuLexer *l, char *fname, fu_size_t len);

fu_bool_t FuLexer_is_eof(FuLexer *l);
FuToken FuLexer_get_token(FuLexer *l);
void FuLexer_unget_token(FuLexer *l, FuToken tok);
FuStr *FuLexer_display_token(FuLexer *l, FuToken token);

FuStr *FuLexer_dump(FuLexer *l);

struct FuScope {
    FuCtx *ctx;
    FuScope *super;
    fu_sym_t name;

    fu_bool_t is_fn;
    FuMap *closure_syms;

    FuMap *declares;
    FuMap *labels;
    FuMap *types;
    FuMap *interfaces;
    FuMap *extensions;
};

FuScope *FuScope_new(FuCtx *ctx, FuScope *super, fu_sym_t name);
void FuScope_drop(FuScope *scp);

FuType *FuScope_get_type(FuScope *scp, fu_sym_t name);

typedef fu_bool_t (*FuCheckTokenFn)(FuToken tok);

struct FuParser {
    FuCtx *ctx;
    FuLexer *lexer;
    /* FuToken */
    FuVec *tok_buf;
    fu_bool_t in_tok_tree;
    /* FuToken */
    FuVec *unclosed_delims;
};

FuParser *FuParser_new(FuCtx *ctx);
void FuParser_drop(FuParser *p);

void FuParser_for_file(FuParser *p, char *fpath, fu_size_t len);

FuLit *FuParser_parse_lit(FuParser *p);
FuIdent *FuParser_parse_ident(FuParser *p);
FuLabel *FuParser_parse_label(FuParser *p);
FuType *FuParser_parse_type(FuParser *p, fu_op_prec_t prec, fu_bool_t check_null);

FuPathItem *FuParser_parse_path_item(FuParser *p);
FuPath *FuParser_parse_path(FuParser *p);
FuPat *FuParser_parse_pat(FuParser *p, fu_op_prec_t prec, fu_bool_t check_null);
FuExpr *FuParser_parse_expr(FuParser *p, fu_op_prec_t prec, fu_bool_t check_null);
FuBlock *FuParser_parse_block(FuParser *p);
FuExpr *FuParser_parse_block_expr(FuParser *p);

fu_vis_k FuParser_parse_visibility(FuParser *p);
FuNode *FuParser_parse_item_static(FuParser *p, FuVec *attrs, fu_vis_k vis);
FuNode *FuParser_parse_item_const(FuParser *p, FuVec *attrs, fu_vis_k vis);
FuNode *FuParser_parse_item_let(FuParser *p, FuVec *attrs);
FuNode *FuParser_parse_item_assign(FuParser *p, FuVec *attts, FuExpr *lexpr);
FuNode *FuParser_parse_item_break(FuParser *p, FuVec *attts);
FuNode *FuParser_parse_item_continue(FuParser *p, FuVec *attts);
FuNode *FuParser_parse_item_yield(FuParser *p, FuVec *attts);
FuNode *FuParser_parse_item_throw(FuParser *p, FuVec *attts);
FuNode *FuParser_parse_item_await(FuParser *p, FuVec *attts);
FuNode *FuParser_parse_item_return(FuParser *p, FuVec *attts);
FuNode *FuParser_parse_item_block(FuParser *p, FuVec *attts);
FuNode *FuParser_parse_item_while(FuParser *p, FuVec *attrs);
FuNode *FuParser_parse_item_for(FuParser *p, FuVec *attrs);
FuNode *FuParser_parse_item_loop(FuParser *p, FuVec *attrs);
FuNode *FuParser_parse_item_if(FuParser *p, FuVec *attrs);
FuNode *FuParser_parse_item_match(FuParser *p, FuVec *attrs);
FuNode *FuParser_parse_item_try(FuParser *p, FuVec *attrs);

FuNode *FuParser_parse_mod_item(FuParser *p);
FuVec *FuParser_parse_mod_items(FuParser *p);

FuNode *FuParser_parse_pkg(FuParser *p);

FuStr *FuParser_dump_tokens(FuParser *p);

struct FuType {
    fu_type_k kd;
    FuSpan *sp;
    fu_tid_t tid;
    fu_vis_k vis;
    fu_bool_t is_infered;
    FuVec *attrs;
    union {
        struct {
            FuAnnoSelf *anno;
            FuPath *path;
        } _path;
        FuType *_ptr;
        FuType *_raw_ptr;
        FuType *_nilable;
        struct {
            FuType *ty;
            FuExpr *size;
        } _array;
        FuType *_slice;
        FuVec *_tuple;
        FuFnSig *_fn_sig;

        struct {
            FuGeneric *ge;
            FuIdent *ident;
            /* FuNode._field_def */
            FuVec *items;
        } _struct;
        struct {
            FuGeneric *ge;
            FuIdent *ident;
            /* FuType */
            FuVec *items;
        } _tuple_struct;
        struct {
            FuGeneric *ge;
            FuIdent *ident;
            /* FuNode._variant */
            FuVec *items;
        } _enum;
        struct {
            FuGeneric *ge;
            FuIdent *ident;
            /* FuNode._field_def */
            FuVec *items;
        } _union;
        struct {
            FuGeneric *ge;
            fu_bool_t is_unsafe;
            FuIdent *ident;
            /* FuNode._... */
            FuVec *assocs;
        } _interface;
        struct {
            FuGeneric *ge;
            FuIdent *ident;
            FuType *ty;
        } _alias;
    };
};

FuType *FuType_new(FuCtx *ctx, FuSpan *sp, fu_type_k kd);
FuType *FuType_from_keyword(FuCtx *ctx, FuSpan *sp, fu_keyword_k keyword);
FuType *FuType_new_path(FuCtx *ctx, FuPath *path);
FuType *FuType_new_fn_sig(FuCtx *ctx, FuSpan *sp, FuFnSig *sig);

void FuType_drop(FuType *ty);
FuStr *FuType_display(FuType *ty);

fu_op_prec_t FuType_precedence(FuType *ty);

void FuType_init_pkg_builtins(FuCtx *ctx, FuNode *nd);

struct FuTokTree {
    FuSpan *sp;
    fu_bool_t is_group;
    union {
        FuToken _token;
        struct {
            FuSpan *open_sp;
            FuSpan *close_sp;
            FuChar delimiter;
            /* FuTokTree */
            FuVec *tokens;
        } _group;
    };
};

struct FuIdent {
    FuSpan *sp;
    fu_sym_t name;
};

FuIdent *FuIdent_new(FuSpan *sp, fu_sym_t sym);
void FuIdent_drop(FuIdent *ident);
FuStr *FuIdent_display(FuIdent *ident);

struct FuLabel {
    FuSpan *sp;
    fu_sym_t name;
};

FuLabel *FuLabel_new(FuSpan *sp, fu_sym_t name);
void FuLabel_drop(FuLabel *label);
FuStr *FuLabel_display(FuLabel *label);

struct FuAttr {
    fu_attr_k kd;
    fu_bool_t is_outer;
    union {
        struct {
            FuPath *path;
            FuTokTree *tok_tree;
        } _normal;
        FuStr *_doc_comment;
    };
};

struct FuBlock {
    FuSpan *sp;
    FuScope *scope;
    /* FuNode */
    FuVec *items;
};

FuBlock *FuBlock_new(FuSpan *sp);
void FuBlock_drop(FuBlock *blk);
FuStr *FuBlock_display(FuBlock *blk, fu_size_t indent);

struct FuUse {
    FuSpan *sp;
    FuStr *prefix;
    fu_use_k kd;
    union {
        FuNode *_alias;
        /* FuUse */
        FuVec *_nested;
    };
};

/* `std`, `Vec#<T>` */
struct FuPathItem {
    FuSpan *sp;
    FuIdent *ident;
    /* FuGeArg */
    FuVec *ge_args;
};

void FuPathItem_drop(FuPathItem *item);
FuStr *FuPathItem_display(FuPathItem *item);

/* `std::vec::Vec#<T>` */
struct FuPath {
    FuSpan *sp;
    /* FuPathItem */
    FuVec *segments;
};

void FuPath_drop(FuPath *path);
FuStr *FuPath_display(FuPath *path);

/*
 * 类型注解
 * 和 `FuPath` 一起使用
 * `idx` 代表 `Fupath` 里面的索引
 *
 * ```
 * <Vec#<T> as a::b::IInterface>::AssocItem
 *  ^~~~~     ~~~~~~~~~~~~~~^
 *  ty        idx = 3
 *
 * Vec#<T>::AssocItem
 *  ^~~~~    ^
 *  ty       idx = 0
 * ```
 */
struct FuAnnoSelf {
    FuSpan *sp;
    FuType *ty;
    fu_size_t idx;
};

struct FuArm {
    fu_arm_k kd;
    FuSpan *sp;
    FuVec *attrs;
    FuPat *pat;
    FuExpr *guard;
    union {
        struct {
            FuExpr *body;
        } _match;
        struct {
            FuBlock *body;
        } _catch;
    };
};

FuArm *FuArm_new(FuSpan *sp, fu_arm_k kd);
void FuArm_drop(FuArm *arm);
FuStr *FuArm_display(FuArm *arm, fu_size_t indent);

struct FuFieldInit {
    fu_field_k kd;
    FuSpan *sp;
    FuVec *attrs;
    FuType *ty;
    union {
        FuExpr *_expr;
        struct {
            FuIdent *ident;
            FuExpr *init;
        } _name;
        FuExpr *_repeat;
        FuExpr *_base;
        struct {
            FuLit *lit;
            FuExpr *init;
        } _index;
        FuExpr *_size;
    };
};

FuFieldInit *FuFieldInit_new(FuSpan *sp, fu_field_k kd, FuVec *attrs);
void FuFieldInit_drop(FuFieldInit *init);
FuStr *FuFieldInit_display(FuFieldInit *init, fu_size_t indent);

/* `i: i32` */
struct FuFnParam {
    FuSpan *sp;
    FuVec *attrs;
    FuIdent *ident;
    FuType *ty;
};

FuFnParam *FuFnParam_new(FuSpan *sp, FuIdent *ident);
void FuFnParam_drop(FuFnParam *param);
FuStr *FuFnParam_display(FuFnParam *param, fu_size_t indent);

/*
 * function signature
 * #<T> [T] -> (T -> bool) -> [T]
 */
struct FuFnSig {
    FuGeneric *ge;
    fu_bool_t is_unsafe;
    fu_bool_t is_const;
    fu_bool_t is_async;
    fu_bool_t is_extern;
    FuVec *tys;
};

FuFnSig *FuFnSig_new(FuGeneric *ge, FuVec *tys);
void FuFnSig_drop(FuFnSig *sig);
FuStr *FuFnSig_display(FuFnSig *sig);

struct FuMacroCall {
    fu_bool_t is_method;
    FuNode *path;
    FuTokTree *args;
};

struct FuGeParam {
    FuSpan *sp;
    fu_ge_param_k kd;
    FuIdent *ident;
    union {
        struct {
            FuType *deflt;
        } _type;
        struct {
            FuType *ty;
        } _const;
    };
};

struct FuGeBound {
    FuSpan *sp;
    FuType *ty;
    /* FuType */
    FuVec *interfaces;
};

struct FuGeneric {
    /* FuGeParam */
    FuVec *params;
    /* FuGeBound */
    FuVec *bounds;
};

struct FuGeArg {
    FuSpan *sp;
    fu_ge_arg_k kd;
    union {
        FuType *_type;
        FuNode *_const;
        struct {
            FuType *param;
            FuType *ty;
        } _binding;
    };
};

struct FuLit {
    FuSpan *sp;
    fu_lit_k kd;
    union {
        struct {
            fu_uint8_t v;
        } _bool;
        struct {
            fu_uint8_t v;
        } _byte;
        struct {
            fu_uint32_t v;
        } _char;
        struct {
            /* 0: unsuffix, 8, 16, 32, 64 */
            fu_uint8_t size;
            fu_bool_t is_signed;
            fu_uint64_t v;
        } _int;
        struct {
            FuToken tok;
            /* 0: unsuffix, 32, 64 */
            fu_uint8_t size;
            union {
                float v32;
                double v64;
            };
        } _float;
        FuStr *_str;
        FuBytes *_byte_str;
        FuStr *_format_str;
    };
};

FuLit *FuLit_new(FuSpan *sp, fu_lit_k kind);
void FuLit_drop(FuLit *lit);
FuStr *FuLit_display(FuLit *lit, fu_size_t indent);

struct FuExpr {
    FuSpan *sp;
    fu_expr_k kd;
    FuType *ty;
    fu_bool_t is_const;
    union {
        FuLit *_lit;
        struct {
            FuAnnoSelf *anno;
            FuPath *path;
        } _path;
        struct {
            /* FuFieldInit */
            FuVec *field_inits;
        } _array;
        struct {
            /* FuFieldInit */
            FuVec *fields;
        } _tuple;
        struct {
            FuExpr *base;
            /* FuFieldInit */
            FuVec *field_inits;
        } _struct;
        struct {
            FuExpr *base;
            /* FuFieldInit */
            FuVec *field_inits;
        } _tuple_struct;
        struct {
            fu_bool_t is_inclusive;
            FuExpr *start;
            FuExpr *end;
        } _range;
        struct {
            FuExpr *base;
            FuIdent *ident;
        } _field;
        struct {
            FuExpr *base;
            FuExpr *idx;
        } _index;
        struct {
            FuExpr *expr;
            FuType *ty;
        } _cast;
        struct {
            FuExpr *base;
            /* FuExpr */
            FuVec *args;
        } _call;
        struct {
            FuPathItem *method;
            /* FuExpr, first arg is self */
            FuVec *args;
        } _method_call;
        struct {
            fu_op_k op;
            FuSpan *op_sp;
            FuExpr *expr;
        } _unary;
        struct {
            fu_op_k op;
            FuSpan *op_sp;
            FuExpr *lexpr;
            FuExpr *rexpr;
        } _binary;
        /* suffix ? expr */
        struct {
            fu_op_k op;
            FuSpan *op_sp;
            FuExpr *expr;
        } _catch;
        struct {
            fu_bool_t is_async;
            fu_bool_t is_unsafe;
            FuLabel *label;
            FuBlock *block;
        } _block;
        struct {
            fu_bool_t is_async;
            fu_bool_t is_unsafe;
            FuScope *scope;
            /* FuFnParam */
            FuVec *params;
            FuExpr *body;
        } _closure;
        struct {
            FuPat *pat;
            FuExpr *expr;
        } _let_cond;
        struct {
            FuExpr *cond;
            FuBlock *block;
            FuExpr *next_if;
        } _if;
        struct {
            FuLabel *label;
            FuBlock *block;
        } _loop;
        struct {
            FuExpr *cond;
            /* FuArm */
            FuVec *arms;
        } _match;
        FuMacroCall *_macro_call;
    };
};

FuExpr *FuExpr_new(FuSpan *sp, fu_expr_k kd);
FuExpr *FuExpr_new_lit(FuLit *lit);
FuExpr *FuExpr_new_path(FuAnnoSelf *anno, FuPath *path);
fu_bool_t FuExpr_can_endwith_semi(FuExpr *expr);

void FuExpr_drop(FuExpr *expr);
FuStr *FuExpr_display(FuExpr *expr, fu_size_t indent);

struct FuPat {
    FuSpan *sp;
    fu_pat_k kd;
    union {
        FuExpr *_expr;
        FuLit *_index;
        FuIdent *_field;
        FuPat *_repeat;
        FuExpr *_base;
        struct {
            fu_bool_t is_ref;
            FuIdent *ident;
            FuPat *pat;
        } _bind;
        struct {
            FuVec *pats;
        } _or;
        struct {
            FuVec *pats;
        } _slice;
        struct {
            FuVec *pats;
        } _tuple;
        struct {
            FuExpr *path;
            FuVec *pats;
        } _struct;
        struct {
            FuExpr *path;
            FuVec *pats;
        } _tuple_struct;
    };
};

FuPat *FuPat_new(FuSpan *sp, fu_pat_k kd);
void FuPat_drop(FuPat *pat);
FuStr *FuPat_display(FuPat *pat, fu_size_t indent);

struct FuNode {
    FuSpan *sp;
    fu_node_k kd;
    /* FuAttr */
    FuVec *attrs;
    fu_nid_t nid;
    fu_bool_t is_inferred;
    union {
        struct {
            FuType *ty;
            FuExpr *expr;
            fu_bool_t end_semi;
        } _expr;
        struct {
            fu_vis_k vis;
            FuUse *tree;
        } _use;
        struct {
            fu_vis_k vis;
            FuIdent *ident;
            FuType *ty;
            FuExpr *init_expr;
        } _static;
        struct {
            fu_vis_k vis;
            FuIdent *ident;
            FuType *ty;
            FuExpr *init_expr;
        } _const;
        struct {
            FuPat *pat;
            FuType *ty;
            FuExpr *init;
        } _let;
        struct {
            fu_op_k op;
            FuSpan *op_sp;
            FuExpr *lexpr;
            FuExpr *rexpr;
        } _assign;
        struct {
            FuLabel *label;
            FuExpr *expr;
        } _break;
        struct {
            FuLabel *label;
        } _continue;
        struct {
            FuExpr *expr;
        } _yield;
        struct {
            FuExpr *expr;
        } _throw;
        struct {
            FuExpr *expr;
        } _await;
        struct {
            FuExpr *expr;
        } _return;
        struct {
            FuLabel *label;
            FuExpr *cond;
            FuBlock *block;
        } _while;
        struct {
            FuLabel *label;
            FuPat *pat;
            FuExpr *expr;
            FuBlock *block;
        } _for;
        struct {
            FuBlock *block;
            /* FuArm */
            FuVec *arms;
            FuBlock *finally;
        } _try;
        struct {
            FuIdent *ident;
            FuScope *scope;
            FuType *ty;
            /* FuFnParam */
            FuVec *params;
            /* FuNode */
            FuVec *body;
        } _fn;
        FuType *_type;
        struct {
            /* FuNode
             *     - _static
             *     - _const
             *     - _type
             *     - _fn
             */
            FuVec *declares;
        } _extern;
        struct {
            FuGeneric *ge;
            FuType *ty;
            FuType *interface;
            /* FuNode
             *     - _const
             *     - _fn
             *     - _type
             *     - _macro_call
             */
            FuVec *assocs;
        } _extension;
        struct {
            fu_vis_k vis;
            FuIdent *ident;
            fu_bool_t is_method;
            FuTokTree *args;
            /* FuTokTree */
            FuVec *patterns;
            /* FuTokTree */
            FuVec *templates;
        } _macro_def;
        FuMacroCall *_macro_call;
        struct {
            fu_vis_k vis;
            FuIdent *ident;
            FuScope *scope;
            /* inner file span` */
            FuSpan *inner_sp;
            /* flase for `mod name;` */
            fu_bool_t is_inline;
            /* FuNode */
            FuVec *items;
        } _mod;
        struct {
            fu_sym_t name;
            /* FuNode */
            FuVec *items;

            FuVec *extern_pkgs;
            FuScope *builtins;
            FuScope *globals;
        } _pkg;
        struct {
            fu_vis_k vis;
            FuIdent *ident;
            FuType *ty;
        } _field_def;
        struct {
            fu_variant_k kd;
            fu_vis_k vis;
            FuIdent *ident;
            /* `name: int` */
            FuVec *fields;
        } _variant;
    };
};

FuNode *FuNode_new(FuCtx *ctx, FuSpan *sp, fu_node_k kind);
void FuNode_drop(FuNode *nd);

FuNode *FuNode_new_expr(FuCtx *ctx, FuExpr *expr);
FuNode *FuNode_new_pkg(FuCtx *ctx, FuSpan *sp);

FuStr *FuNode_display(FuNode *nd, fu_size_t indent);

#endif /* FU_PARSE_H */
